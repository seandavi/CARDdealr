#' get the BiocFileCache for CARDdealr
#'
#' Set up or get location of BiocFileCache. By default,
#' this uses the name BiocFileCache.
#'
#' @importFrom BiocFileCache BiocFileCache bfcneedsupdate bfcupdate bfcadd bfcquery bfcrpath
#'
#' @param cache character(1), the path to the cache directory. See [BiocFileCache::BiocFileCache()].
#'
#' @return a `BiocFileCache` instance
#'
#' @keywords internal
cdr_get_cache <- function(cache = rappdirs::user_cache_dir(appname='CARDdealr')) {
  BiocFileCache::BiocFileCache(cache=cache)
}

#' @importFrom BiocFileCache bfcneedsupdate bfcdownload bfcadd bfcquery bfcrpath
#'
cdr_cached_url <- function(url, rname = url, ask_on_update=FALSE,
                           max_cache_age=getOption('CARDdealr.max_cache_age', '30 days'),
                           ...) {
  bfc = cdr_get_cache()
  bfcres = bfcquery(bfc,rname,'rname')

  rid = bfcres$rid
  # Not found
  fileage = 0
  if(!length(rid)) {
    rid = names(bfcadd(bfc, rname, url))
  } else {
    fileage = lubridate::as_datetime(Sys.time()) -
      lubridate::parse_date_time2(bfcres$access_time, "YmdHMS", tz=Sys.timezone())
  }
  # if needs update, do the download
  if(isTRUE(bfcneedsupdate(bfc, rid))) {
    bfcdownload(bfc, rid, ask=FALSE, ...)
  } else if (fileage > lubridate::as.period(max_cache_age)) {
    bfcdownload(bfc, rid, ask=FALSE, ...)
  }
  bfcrpath(bfc, rids = rid)
}
