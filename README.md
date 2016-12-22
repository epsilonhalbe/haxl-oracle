# haxl-oracle

Use this with [docker: wnameless/oracle-xe-11g/](https://hub.docker.com/r/wnameless/oracle-xe-11g/)

```
> git clone https://github.com/epsilonhalbe/haxl-oracle
> cd haxl-oracle
```

Download the linux drivers for oracle 11.2 [oracle](http://www.oracle.com/technetwork/topics/linuxx86-64soft-092277.html)
to `oracle-utils/11.2/` and convert the `*.rpm` files to `*.deb` with the help of `alien`.

```
> docker build .
```

Copy and paste the hash of the docker build to the `image`-section in the [`stack.yaml`](./stack.yaml)-file.

```
stack ghci
```

should then run/compile inside the docker container - for a basic example see - `BlogDB.hs`
