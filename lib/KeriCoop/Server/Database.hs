-- | SQLite storage for append-only event logs.
module KeriCoop.Server.Database (
    Database (..),
    initDatabase,
    createGroup,
    appendEvent,
    getEvents,
    AppendError (..),
) where

import Control.Exception (catch)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (
    Connection,
    SQLError,
    execute,
    execute_,
    open,
    query,
 )

-- | Handle to the database.
newtype Database = Database {dbConn :: Connection}

-- | Error when appending an event.
data AppendError
    = -- | The sequence number already exists.
      SeqConflict
    deriving stock (Show)

-- | Open (or create) the database and initialize the schema.
initDatabase :: FilePath -> IO Database
initDatabase path = do
    conn <- open path
    execute_ conn $
        "CREATE TABLE IF NOT EXISTS events ("
            <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
            <> "group_id TEXT NOT NULL,"
            <> "seq INTEGER NOT NULL,"
            <> "payload TEXT NOT NULL,"
            <> "created_at INTEGER DEFAULT (unixepoch()),"
            <> "UNIQUE(group_id, seq)"
            <> ")"
    pure Database{dbConn = conn}

-- | Create a group. Currently a no-op since groups are implicit.
createGroup :: Database -> Text -> IO ()
createGroup _db _groupId = pure ()

{- | Append an event to a group's log. Returns the assigned sequence
number, or 'SeqConflict' if the sequence number is already taken.
-}
appendEvent ::
    Database -> Text -> Int64 -> Text -> IO (Either AppendError Int64)
appendEvent Database{..} groupId seqNo payload = do
    let q =
            "INSERT INTO events (group_id, seq, payload) "
                <> "VALUES (?, ?, ?)"
    (Right seqNo <$ execute dbConn q (groupId, seqNo, payload))
        `catch` \(_ :: SQLError) -> pure (Left SeqConflict)

-- | Fetch events for a group after a given sequence number.
getEvents :: Database -> Text -> Int64 -> IO [(Int64, Text)]
getEvents Database{..} groupId afterSeq =
    query
        dbConn
        "SELECT seq, payload FROM events \
        \WHERE group_id = ? AND seq > ? ORDER BY seq"
        (groupId, afterSeq)
