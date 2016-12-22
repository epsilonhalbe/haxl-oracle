from library/haskell:8.0.1
run apt-get update && apt-get install -y \
  unixodbc unixodbc-dev libaio1 alien
add ./oracle-utils /root/oracle-utils/
run alien -i oracle-instantclient12.1-basic-12.1.0.2.0-1.x86_64.rpm
run alien -i oracle-instantclient12.1-sqlplus-12.1.0.2.0-1.x86_64.rpm
run alien -i oracle-instantclient12.1-devel-12.1.0.2.0-1.x86_64.rpm
run cat /root/oracle-utils/_odbc.ini > ~/.odbc.ini
