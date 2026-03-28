library(DBI)
library(RPostgres)
source("wrdsconnect.R")

wrds <- connect()

print(wrds)

try(DBI::dbGetQuery(wrds, "SELECT 1 AS x"))
try(DBI::dbGetQuery(wrds, "SELECT current_user, current_database()"))
try(DBI::dbGetQuery(wrds, "SELECT * FROM taqmsec.ctm_20240102 LIMIT 1"))

try(DBI::dbGetQuery(wrds, "SELECT * FROM optionm_all.secprd LIMIT 5"))
try(DBI::dbGetQuery(wrds, "SELECT * FROM optionm_all.opprcd LIMIT 5"))

DBI::dbGetQuery(wrds, "
  SELECT tablename
  FROM pg_catalog.pg_tables
  WHERE schemaname = 'optionm_all'
  ORDER BY tablename
")


DBI::dbGetQuery(wrds, "
  SELECT schemaname, tablename
  FROM pg_catalog.pg_tables
  WHERE schemaname = 'optionm_all'
    AND (
      tablename ILIKE '%op%'
      OR tablename ILIKE '%vol%'
      OR tablename ILIKE '%surf%'
      OR tablename ILIKE '%price%'
    )
  ORDER BY tablename
")



DBI::dbGetQuery(wrds, "
  SELECT column_name, data_type
  FROM information_schema.columns
  WHERE table_schema = 'optionm_all'
    AND table_name = 'securd'
  ORDER BY ordinal_position
")

DBI::dbGetQuery(wrds, "
  SELECT column_name, data_type
  FROM information_schema.columns
  WHERE table_schema = 'optionm_all'
    AND table_name = 'opprcd2024'
  ORDER BY ordinal_position
")

DBI::dbGetQuery(wrds, "
  SELECT column_name, data_type
  FROM information_schema.columns
  WHERE table_schema = 'optionm_all'
    AND table_name = 'vsurfd2024'
  ORDER BY ordinal_position
")