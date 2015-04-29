#!/bin/bash -x

sudo apt-get install postgresql mysql-server -y

sudo -u postgres psql <<EOF
CREATE USER "PROJECTNAME_LOWER" password 'PROJECTNAME';
CREATE DATABASE "PROJECTNAME_LOWER_test" OWNER "PROJECTNAME";
EOF

sudo mysql -u root -p <<EOF
CREATE DATABASE PROJECTNAME_test;
GRANT USAGE ON *.* TO PROJECTNAME@localhost IDENTIFIED BY 'PROJECTNAME';
GRANT ALL PRIVILEGES ON PROJECTNAME_test.* TO PROJECTNAME@localhost;
EOF
