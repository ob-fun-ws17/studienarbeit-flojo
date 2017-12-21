module Response(
    ResponseLine(..)
) where
  import StatusCode
  data ResponseLine = ResponseLine String StatusCode
  instance Show ResponseLine where
    show ( ResponseLine version status) = version ++ " " ++ show (code status) ++ " " ++ reason status
