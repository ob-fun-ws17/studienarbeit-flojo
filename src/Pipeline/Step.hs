module Step(
  Step
)
where

class Step where
  invoke :: Request -> Either PipelineError Response

class Middleware where
  invoke :: Step -> (Request -> Either PipelineError Response)

testHandler :: Step -> (Request -> Either PipelineError Response)
testHandler step =  case (invoke step) of
    Left error -> Left error
    Right x ->  Right X
data PipelineError = Aborted String
