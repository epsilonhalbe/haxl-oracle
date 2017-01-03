# haxl-oracle

## Preparations - set up an oracle experiment database (dockerized)

Use this with [docker: wnameless/oracle-xe-11g/](https://hub.docker.com/r/wnameless/oracle-xe-11g/) - you can start the container by running

first time 
```
> docker run -d -p 49160:22 -p 49161:1521 --name oracle wnameless/oracle-xe-11g
```
or any time later

```
> docker start oracle
```

## Step 2: create the Haskell environment to work with

This repository is designed to run (almost) on its own, you need [stack](https://docs.haskellstack.org/en/stable/README/) and
the oracle drivers (see below)

But first we start by cloning this repository:

```
> git clone https://github.com/epsilonhalbe/haxl-oracle
> cd haxl-oracle
```

Then download the linux drivers for oracle 11.2 [oracle](http://www.oracle.com/technetwork/topics/linuxx86-64soft-092277.html)
to `oracle-utils/11.2/` (note: you need an oracle account for that) then

```
> docker build --force-rm -t haskel:8.0.1-oracle .
```

and everything should be ready

```
> stack build
> stack exec -- isql ORACLE11
> stack ghci
```


should all work. You can use the second one to insert the contents of
`src/createblog.sql` to have some test data.

for a basic example see - `BlogDB.hs`.

#Thanks

Thanks go to Simon Marlow and Facebook for creating haxl - and open sourcing it.


