# Whack a Fraudster

A simple game in Elm and javascript

This week we'll be discussing and comparing a javascript implementation for a simple whack-a-mole style game and the implementation for the same game in Elm. After going over the pros, cons and general observations of each implementation we'll get our hands dirty extending the elm implementation.

You can find a link to the game on ellie below:
https://ellie-app.com/j3zdd4RvQa1/5

# Get started

Feel free to extend the game in anyway you like, if nothing comes to mind then why not build the functionality for a super customer rather than a super fraudster and accidentally clicking the super customer could be a huge point penalty or even end the game?

#So what do we need to look at?

**Adding an entry to the _ContentType_**

```elm
type ContentType
    = Empty
    | Fraudster
    | SuperFraudster
    | Client
```

**Handle clicking the new content type**

We'll need to add a new entry in the case statement as was done for the *SuperFraudster*. There we can either update the score accordingly to how we want this to influence the game or we can call update with the *GameEnded* function to have a 'Game Over' style action.

```elm
ClickBox index ->
    let
        ( fraudsters, customers, superbadGuy ) =
            model.score
    in
    case Dict.get index model.gridContents of
        Just SuperFraudster ->
            ( { model
                | score = ( fraudsters, customers, superbadGuy + 1 )
                , gridContents =
                    Dict.update
                        index
                        (Maybe.map (\previousContentTypes -> Empty))
                        model.gridContents
              }
            , Cmd.none
            )
```

**Add to the model and initialise in _StartGame_**

```elm
StartGame ->
    let
        ( updatedState, updatedCmd ) =
            update
                ApplyTick
                { model
                    | gameState = Playing
                    , level = Just Level1
                    , score = ( 0, 0, 0 )
                    , tickCount = 0
                    , superBadGuyTick = Nothing
                }
    in
    ( updatedState, Cmd.batch [ updatedCmd, Task.perform StartedTime Time.now ] )
```


**Re-use or create a new function to randomly allocate when the super customer appears**

```elm
StartedTime time ->
    ( { model
        | startedTime = Just time
        , superBadGuyTick = Just (randomTick (floor time))
      }
    , Cmd.none
    )
```
