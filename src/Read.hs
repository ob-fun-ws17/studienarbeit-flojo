module Read (
  read
)
  where
    import Data.ByteString

    read :: FilePath -> IO ByteString
    read path = readFile path
