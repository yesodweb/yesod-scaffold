.PHONY: check clean seed db dev

EXE_NAME := PROJECTNAME
base_db_name = PROJECTNAME

all: db db_test check seed dev

check:
	stack test --fast

clean:
	stack clean
	stack --docker clean

seed:
	stack exec seed

ghci:
	stack ghci --ghci-options -fobject-code PROJECTNAME:lib

dev:
	stack exec -- yesod devel

db:
	psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '$(base_db_name)'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE $(base_db_name)"

db_down:
	psql -U postgres -c "DROP DATABASE $(base_db_name)"

db_test:
	psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '$(base_db_name)_test'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE $(base_db_name)_test"

db_user:
	createuser -P -s -e $(base_db_name)

db_reset: db_down db seed
