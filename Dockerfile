from library/haskell:8.0.1
run apt-get update && apt-get install -y \
  unixodbc unixodbc-dev libaio1
add ./oracle-utils /root/oracle-utils/
run dpkg -i /root/oracle-utils/11.2/oracle-instantclient11.2-basic_11.2.0.4.0-2_amd64.deb
run dpkg -i /root/oracle-utils/11.2/oracle-instantclient11.2-odbc_11.2.0.4.0-2_amd64.deb
run dpkg -i /root/oracle-utils/11.2/oracle-instantclient11.2-sqlplus_11.2.0.4.0-2_amd64.deb
run cat /root/oracle-utils/_odbc.ini > ~/.odbc.ini
