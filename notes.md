# Notes

## Haskell Notes

The {-# SOURCE #-} pragma is used to stop module loops. These cycles must be broken by a [hs-boot file](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules)

## TODO

- TODO EnemyMsgPayload
- TODO MsgOrder Semantics, see [src/AppEnv/Types.hs](src/AppEnv/Types.hs#L76)

## Message System

Messages for different systems are split up across different data types in Payload.hs](src/Msg/Payload.hs). They are also [phased with read write capabilities per monad](src/Msg/Phase.hs).

These systems include:
  - Audio
  - Collision
  - Console
  - Gameplay events
    - Enemy
    - Hurt
    - Info
    - Room
    - Player
    - Projectile

  - Menu
  - Particle System
  - UI
  - Think Systems ex:
    - NewThinkProjectile
    - NewUpdateProjectile
  - World
    - LockCamera
    - Pause
    - Screenshake
    - Hitlag

These then all get instances for IsMsgPayload.

Finally theres an [overall MsgPayload](src/Msg/Payload.hs#L413) typ that bundles these all together.

Enemy and Player use GADTs and Typeable. There's also this weird GADT called Some from Util.hs

```haskell
data Some a where
    Some :: a d -> Some a
```

Looks like it hides a data type paramater for things like projectiles.

The [message phase module](src/Msg/Phase.hs) handles instances for AllowMsgRead/AllowMsgWrite for message types at certain phases.

```haskell
class IsMsgPayload a => AllowMsgRead p a where
class IsMsgPayload a => AllowMsgWrite p a where

instance AllowMsgRead SetupMsgsPhase ConsoleMsgPayload where
instance AllowMsgWrite SetupMsgsPhase ConsoleMsgPayload where
```

- [Message Ordering](src/Msg/Types.hs#L18)
- [Message Payload](src/Msg/Payload.hs)
- [Message Phase](src/Msg/Phase.hs)
- [MsgsRead and MsgsWrite monad typeclass](src/Msg/Types.hs#L32)

### Thinking and Messages

Here is the path from main to thinkEnemys

[src/Game.hs](src/Game.hs)
```haskell
--Gets called in runGame in Game.hs
gameMain :: AppEnvData -> Window -> Game -> IO ()
gameMain appEnvData window game = do
    (window', configs, game') <- runAppEnv appEnvData $ do
        (win, cfgs, gm) <- updateGame window game
  .....
                    gm'         <- stepGame win' cfgs gm              -- <------------

stepGame :: Window -> Configs -> Game -> AppEnv BaseMsgsPhase Game
stepGame window cfgs game =
  .....
        withAppEnvReadData inputState' cfgs $ case _mode game of
            WorldMode       -> worldMain game                         -- <------------
            PauseMenuMode   -> pauseMenuMain game
            MainMenuMode    -> mainMenuMain game
            UnlocksMenuMode -> unlocksMenuMain game

```

[src/World/Main.hs](src/World/Main.hs)
```haskell
updateWorld :: GameMode -> World -> AppEnv BaseMsgsPhase World
updateWorld prevGameMode world =
  let ...
  in do
    ...
    withMsgsPhase @ThinkEnemyMsgsPhase (thinkEnemyManager enemyManager)   -- <------------
    ...


worldMain :: Game -> AppEnv BaseMsgsPhase Game
  ........
                world'   <- updateWorld (_prevMode game) world            -- <------------
```

[src/Enemy/Manager.hs](src/Enemy/Manager.hs)
```haskell
thinkEnemyManager :: EnemyManager -> AppEnv ThinkEnemyMsgsPhase ()
thinkEnemyManager enemyManager = sequenceA_ [thinkEnemy e | Some e <- enemies] -- <------------
    where enemies = _enemies (enemyManager :: EnemyManager)
```

The meat and potatos.

[src/Enemy.hs](src/Enemy.hs)
```haskell
thinkEnemy :: Enemy d -> AppEnv ThinkEnemyMsgsPhase ()
thinkEnemy enemy
    | isEnemyInStasis enemy = do
        writeMsgs $ lockOnReticleDataMsgs enemy
        writeMsgs [enemyStasisDataSoundMessage (E._pos enemy) (_stasisData enemy)]

    | otherwise = do
        writeMsgs =<< (_thinkAI enemy) enemy
        writeMsgs $ lockOnReticleDataMsgs enemy
        writeMsgs $ maybe [] thinkAttack (_attack enemy)
```

[src/Attack.hs](src/Attack.hs)
```haskell
thinkAttack :: (AllowMsgWrite p AudioMsgPayload, AllowMsgWrite p WorldMsgPayload) => Attack -> [Msg p]
thinkAttack atk = screenshakeMsgs ++ attackSoundMessages atk
  ...

attackSoundMessages :: AllowMsgWrite p AudioMsgPayload => Attack -> [Msg p]
attackSoundMessages atk = case _type (attackSound atk :: AttackSound) of
    AttackPlaySound soundFilePath frameIndex ...
    AttackPlaySounds soundFilePath frameIndices ...
    AttackPlaySoundContinuous soundFilePath soundContinuousData -> ...
    AttackNoSound -> []
```

### WithMsgsPhase

Note: BaseMsgsPhase

[src/AppEnv.hs](src/AppEnv.hs#L101)
```haskell
withMsgsPhase :: AppEnv p a -> AppEnv BaseMsgsPhase a
withMsgsPhase (AppEnv appEnv) = AppEnv appEnv
```

[src/World/Main.hs](src/World/Main.hs#L33)
```haskell
updateWorld :: GameMode -> World -> AppEnv BaseMsgsPhase World
updateWorld prevGameMode world =
    let ...
    in do
        withMsgsPhase @ThinkInfoMsgsPhase (thinkInfoMsgManager world)
        withMsgsPhase @ThinkPlayerMsgsPhase (thinkPlayer player)
        ...

        player'            <- withMsgsPhase @UpdatePlayerMsgsPhase (updatePlayer player)
        enemyManager'      <- withMsgsPhase @UpdateEnemyMsgsPhase (updateEnemyManager enemyManager)
```

[src/Player/Update.hs](src/Player/Update.hs#L608)
```haskell
updatePlayer :: Player -> AppEnv UpdatePlayerMsgsPhase Player
updatePlayer player
    | isPlayerInSpawnAnim player   = updatePlayerInSpawnAnim player
    | isPlayerInDeathAnim player   = updatePlayerInDeathAnim player
    | isPlayerInWarpOutAnim player = updatePlayerInWarpOutAnim player

    | otherwise = do
        inputState <- if
            | _warpingOut (_flags player) -> inactivateInputState <$> readInputState
            | otherwise                   -> readInputState

        player' <- flip execStateT player $ do
            let oldPrevHitbox = _prevHitbox player

            modify $ \p -> p
                { _timersCounters = updatePlayerTimersCounters (_flags p) (_timersCounters p)
                , _flags          = updatePlayerFlags $ _flags p
                , _prevHitbox     = playerHitbox p
                }

            get >>= lift . updatePlayerMessages oldPrevHitbox >>= put
            modify $ updatePlayerMoveLeftRight inputState
            get >>= lift . updatePlayerAimTarget inputState >>= put
            get >>= lift . updatePlayerJump inputState >>= put
            modify updatePlayerGravity

            get >>= \p -> case _movementSkill p of
                Just (Some ms) -> do
                    ms' <- lift $ updateMovementSkill p ms
                    put $ p {_movementSkill = Just (Some ms')}
                Nothing        -> return ()
```

[src/Player/Messages.hs](src/Player/Messages.hs#L57)
```haskell
updatePlayerMessages :: Hitbox -> Player -> AppEnv UpdatePlayerMsgsPhase Player
updatePlayerMessages oldPlayerPrevHbx player =
    processCollisionMsgs oldPlayerPrevHbx player >>=
    processPlayerMsgs >>=
    processHurtMsgs
```

Note: toDyn usage in `update'` and `Typeable`.

[src/Player/Messages.hs](src/Player/Messages.hs#L221)
```haskell
processPlayerMsgs :: Player -> AppEnv UpdatePlayerMsgsPhase Player
processPlayerMsgs player = foldlM processMsg player =<< readMsgs
    where
        processMsg :: Player -> PlayerMsgPayload -> AppEnv UpdatePlayerMsgsPhase Player
        processMsg !p d = case d of
            PlayerMsgSetVelocity vel                    -> return (p {_vel = vel} :: Player)
            PlayerMsgUpdateVelocity update              -> return (p {_vel = update $ _vel (p :: Player)} :: Player)
            PlayerMsgSetDirection dir                   -> return (p {_dir = dir} :: Player)
            PlayerMsgSetPosition pos                    -> return (p {_pos = pos} :: Player)
            .....
            where
                updatePlayerMovementSkill :: Typeable d => (MovementSkill d -> MovementSkill d) -> Player
                updatePlayerMovementSkill update = p {_movementSkill = update' <$> _movementSkill p}
                    where update' = \(Some ms) -> Some $ (MS._updateDynamic ms) (toDyn update) ms

                updatePlayerSecondarySkill
                    :: Typeable d
                    => SecondarySkillSlot
                    -> (SecondarySkill d -> SecondarySkill d)
                    -> Player
                updatePlayerSecondarySkill slot update = p {_secondarySkillManager = secondarySkillMgr'}
                    where
                        update'            = \(Some ss) -> Some $ (SS._updateDynamic ss) (toDyn update) ss
                        secondarySkillMgr  = _secondarySkillManager p
                        secondarySkillMgr' = case slot of
                            SecondarySkillNeutralSlot ->
                                secondarySkillMgr {_neutralSlot = update' <$> _neutralSlot secondarySkillMgr}
                            SecondarySkillUpSlot      ->
                                secondarySkillMgr {_upSlot = update' <$> _upSlot secondarySkillMgr}
                            SecondarySkillDownSlot    ->
                                secondarySkillMgr {_downSlot = update' <$> _downSlot secondarySkillMgr}

                updatePlayerAttackM = \update -> do
                    atk <- maybe (return Nothing) (fmap Just . update) (_attack p)
                    return $ p {_attack = atk}

                resetPlayerRisingJump              = p {_flags = flags {_risingJump = PlayerNotRisingJumpFlag}}
                resetPlayerAirStall                = p
                    { _timersCounters = (_timersCounters p) {_airStallAttacksCounter = 0}
                    }
                resetPlayerMovementSkillNumCharges = p
                    { _movementSkill = _movementSkill p <&> \(Some ms) ->
                        Some $ ms {_numCharges = playerMovementSkillMaxNumCharges p}
                    }

                forcePlayerInAir = p
                    { _flags          = flags {_touchingGround = False}
                    , _timersCounters = (_timersCounters p) {_graceJumpTtl = 0.0}
                    }

                flags = _flags p
```

For reference heres what `Player` looks like:

[src/Player/Types.hs](src/Player/Types.hs#L33)
```haskell
data Player = Player
    { _msgId                 :: MsgId
    , _pos                   :: Pos2
    , _vel                   :: Vel2
    , _dir                   :: Direction
    , _prevHitbox            :: Hitbox
    ...
    }
```

After all of this we can look back at `updateWorld`

It runs in AppEnv and do-binds player' to the returned player and passes it to the new world state world'. Then the world does an update message phase.

[src/World/Main.hs](src/World/Main.hs#L71)
```haskell
updateWorld :: GameMode -> World -> AppEnv BaseMsgsPhase World
updateWorld prevGameMode world =
  ...
        player'            <- withMsgsPhase @UpdatePlayerMsgsPhase (updatePlayer player)
        ...
            world' = world
                { _player            = player'
                ...
                }
        withMsgsPhase @UpdateWorldMsgsPhase (updateWorldMessages world')
```

### AppEnv Transformer Stack

[src/AppEnv/Types.hs](src/AppEnv/Types.hs#L50)

```haskell
newtype AppEnv p a = AppEnv (ReaderT AppEnvReadData (StateT AppEnvWriteData IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppEnvReadData
        , MonadState AppEnvWriteData
        , MonadIO
        , MonadCatch
        , MonadThrow
        , MonadRandom
        )
```

It also contains `MsgsRead` and `MsgsWrite` instances.

[src/AppEnv/Types.hs](src/AppEnv/Types.hs#L70)

Note the message order comparison.

```
instance MsgsRead p (AppEnv p) where
    readMsgsTo :: AllowMsgRead p a => MsgId -> AppEnv p [a]
    readMsgsTo toId = do
        msgs <- M.findWithDefault [] toId <$> gets _messages
        return . catMaybes $ map (fromMsgPayload . _payload) msgs

instance MsgsWrite p (AppEnv p) where
    writeMsgs :: [Msg p] -> AppEnv p ()
    writeMsgs msgs = for_ msgs $ \(Msg msg) ->
        let
            msgToId     = _to msg
            msgOrderCmp = \m1 m2 -> compare (_order m1) (_order m2)
        in do
            msgsList     <- M.findWithDefault [] msgToId <$> gets _messages
            let msgsList' = L.insertBy msgOrderCmp msg msgsList
            modify $ \appEnvData ->
                appEnvData {_messages = M.insert msgToId msgsList' (_messages appEnvData)}

```

The messages live in `_messages` in of the `StateT AppEnvWriteData` part of the monad transformer stack.

[src/AppEnv/Types.hs](src/AppEnv/Types.hs#L41)
```haskell
data AppEnvWriteData = AppEnvWriteData
    { _messages :: M.Map MsgId [MsgInternal]
    }
```

TODO MsgId semantics?
