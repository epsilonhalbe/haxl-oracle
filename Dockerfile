from library/haskell:8.0.1
run apt-get update && apt-get install -y \
  unixodbc unixodbc-dev libaio1 alien
add ./oracle-utils /root/oracle-utils/
run alien -i /root/oracle-utils/11.2/oracle-instantclient11.2-basic_11.2.0.4.0-2_amd64.rpm
run alien -i /root/oracle-utils/11.2/oracle-instantclient11.2-odbc_11.2.0.4.0-2_amd64.rpm
run cat /root/oracle-utils/_odbc.ini > /etc/odbc.ini
env LD_LIBRARY_PATH /usr/lib/oracle/11.2/client64/lib/:$LD_LIBRARY_PATH
