
type LocalState = LocalState { stuff :: TMVar Data.Map key val }


main = do 
    context <- launch "localhost" 1234
    let state = LocalState $ newTMvar $ LocalState $ singleton "key" "val"
    mongoLoop (foo state) isValid context 
    where foo st ctx = 
        do stuff
           mongoInsert ctx (handleInsertReply state ctx) createBSONInsert "value" "full.collection.name"
           mongoSelect ctx (\rs -> mapM putStrLn (documents rs)) 
           exit ctx

handleInsertReply :: LocalState -> MongoContext -> MongoResponse -> IO ()
handleInsert r c = do stuff to handle the response from the server
                      createNewRequest >>= (atomically . writeTChan c) 

handleInsertReply :: LocalState -> MongoContext -> MongoResponse -> IO ()
handleInsert r c = do stuff to handle the response from the server
                      createNewRequest >>= (atomically . writeTChan c) 
    
forkIO $ start prg userChan sock `catch` handler `finally` hClose sock
        where
            handler (IOException e)
                | isEOFError e = return ()
            handler e = putStrLn $ show e

