package("exports")
stdlib("std")

local api = target("api", "library")
api:match("api.chl")
api:headers("api.chl")
