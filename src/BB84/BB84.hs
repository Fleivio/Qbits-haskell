module BB84.BB84 (bb84, bb84Interf, sequentialBb84, sequentialBb84Interf, discardDifferentBasis, verifyInterference) where

import BB84.Person
import BB84.Generator
import Control.Concurrent
import Reference
import Data.IORef (writeIORef)

genQbits :: PersonRef -> Chan (QR Bool) -> Int -> IO()
genQbits (PersonRef aliceRef) qChan size = do
  basis <- genBaseSet size
  bits <- genBitSet size
  let alice = Person basis bits
  writeIORef aliceRef alice

  qbits <- prepareQbits bits basis
  writeList2Chan qChan qbits


interceptQbits :: PersonRef -> Chan (QR Bool) -> Chan (QR Bool) -> Int -> IO()
interceptQbits (PersonRef trudyRef) inChan outChan size = do
  basis <- genBaseSet size
  observations <- readVecFromChan basis size
  let trudy = Person basis observations
  writeIORef trudyRef trudy

  where readVecFromChan [] _ = return []
        readVecFromChan _ 0 = return []
        readVecFromChan (b:bs) sz = do
          qRef <- readChan inChan
          observation <- observeAtBasis b qRef
          writeChan outChan qRef
          obsTail <- readVecFromChan bs (sz - 1)
          return $ observation : obsTail

receiveQbits :: PersonRef -> Chan (QR Bool) -> Int -> IO()
receiveQbits (PersonRef bobRef) chan size = do
  basis <- genBaseSet size
  observations <- readVecFromChan basis size
  let bob = Person basis observations
  writeIORef bobRef bob

  where readVecFromChan [] _ = return []
        readVecFromChan _ 0 = return []
        readVecFromChan (b:bs) sz = do
          qRef <- readChan chan
          observation <- observeAtBasis b qRef
          obsTail <- readVecFromChan bs (sz - 1)
          return $ observation : obsTail

bb84 :: Int -> IO (Person, Person)
bb84 size = do
  aliceRef <- mkPersonRef
  bobRef <- mkPersonRef
  chan <- newChan
  _ <- forkIO (genQbits aliceRef chan size)
  _ <- forkIO (receiveQbits bobRef chan size)
  threadDelay 20000
  alice <- getPerson aliceRef
  bob <- getPerson bobRef
  return (alice, bob)

bb84Interf :: Int -> IO (Person, Person, Person)
bb84Interf size = do
  aliceRef <- mkPersonRef
  bobRef <- mkPersonRef
  trudyRef <- mkPersonRef
  chan1 <- newChan
  chan2 <- newChan
  _ <- forkIO (genQbits aliceRef chan1 size)
  _ <- forkIO (interceptQbits trudyRef chan1 chan2 size)
  _ <- forkIO (receiveQbits bobRef chan2 size)
  threadDelay 20000
  alice <- getPerson aliceRef
  bob <- getPerson bobRef
  trudy <- getPerson trudyRef
  return (alice, trudy, bob)

sequentialGenQbits :: Int -> IO (Person, [QR Bool])
sequentialGenQbits n = do
  basis <- genBaseSet n
  bits <- genBitSet n
  qbits <- prepareQbits bits basis
  let alice = Person basis bits
  return (alice, qbits)

sequentialReceiveQbits :: [QR Bool] -> IO Person
sequentialReceiveQbits inputQbits = do
  basis <- genBaseSet (length inputQbits)
  let obsIO = sequence [observeAtBasis base qref | (base, qref) <- zip basis inputQbits]
  obs <- obsIO
  let bob = Person basis obs
  return bob

sequentialBb84 :: Int -> IO (Person, Person)
sequentialBb84 size = do
  (alice, qbits) <- sequentialGenQbits size
  bob <- sequentialReceiveQbits qbits
  return (alice, bob)

sequentialBb84Interf :: Int -> IO (Person, Person, Person)
sequentialBb84Interf size = do
  (alice, qbits) <- sequentialGenQbits size
  trudy <- sequentialReceiveQbits qbits
  bob <- sequentialReceiveQbits qbits
  return (alice, trudy, bob)

discardDifferentBasis :: Person -> Person -> ([Bool], [Bool])
discardDifferentBasis alice bob = (aliceKey, bobKey)
  where
    aliceKey = [ bit | (a, b, bit) <- zipQbits alice bob, a == b ]
    bobKey = [ bit | (b, a, bit) <- zipQbits bob alice, b == a ]
    zipQbits p1 p2 = zip3 (readBasis p1) (readBasis p2) (obsRes p1)

verifyInterference :: Person -> Person -> IO String
verifyInterference alice bob = do
  let (aliceK, bobK) = discardDifferentBasis alice bob
      hadInterf = False `elem` ([ a == b | (a,b) <- zip aliceK bobK ])
  if hadInterf then return "Houve interferência"
  else return $ show aliceK