{-# LANGUAGE OverloadedStrings #-}
module Constants (
  mainTorrents,
  basicSession,
  arrPaths,
  labels,
  pathMap
  )
where

import           Data.Text (Text)

mainTorrents :: [Text]
mainTorrents = ["name", "downloadedEver", "rateDownload", "uploadedEver", "rateUpload", "eta", "uploadRatio", "totalSize", "peersConnected", "webseeds", "dateCreated", "status", "percentDone", "labels", "error", "id", "files", "downloadDir", "percentComplete", "peers"]

basicSession :: [Text]
basicSession = ["speed-limit-down-enabled", "speed-limit-down", "speed-limit-up-enabled", "speed-limit-up"]

arrPaths :: [(FilePath, FilePath)]
arrPaths = [("/opt/movies", "/opt/movies/finished"), ("/opt/shows", "/opt/shows/finished"), ("/opt/restricted", "/opt/restricted/finished"), ("/opt/calibre", "/opt/calibre/Calibre_Library"), ("/opt/calibre", "/opt/calibre/Comics")]

labels :: [Text]
labels = ["radarr", "sonarr", "cross-seed"]

pathMap :: [(FilePath, FilePath)]
pathMap = [
  ("/var/lib/rtorrent/downloads/radarr", "/opt/movies/downloads"),
  ("/var/lib/rtorrent/movies_uploads", "/opt/movies/uploads"),
  ("/var/lib/rtorrent/movies_finished", "/opt/movies/finished"),
  ("/var/lib/rtorrent/downloads/sonarr", "/opt/shows/downloads"),
  ("/var/lib/rtorrent/shows_uploads", "/opt/shows/uploads"),
  ("/var/lib/rtorrent/music_uploads", "/opt/music/uploads"),
  ("/var/lib/rtorrent/downloads/lidarr", "/opt/music/downloads"),
  ("/var/lib/rtorrent/downloads/mam", "/opt/calibre/mam"),
  ("/var/lib/rtorrent/downloads/readarr", "/opt/calibre/downloads"),
  ("/var/lib/rtorrent/book_uploads", "/opt/calibre/uploads"),
  ("/var/lib/rtorrent/adult_finished", "/opt/restricted/finished"),
  ("/var/lib/rtorrent/adult_uploads", "/opt/restricted/uploads"),
  ("/var/lib/rtorrent/downloads/whisparr", "/opt/restricted/downloads")
                        ]
