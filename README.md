# APSIM Parameteriser

This is a client for the Apsim Server, which is a new feature in ApsimX.

To run a local build of ApsimX as the server on a simulation of your choice:

```
~\src\ApsimX\bin\Release\netcoreapp3.1> dotnet .\apsim-server.dll listen --file my-simulation.apsimx --keep-alive -b 1 -n -r -v
```

To build and run this client:

```
cabal build
cabal exec apsim-params
```

## TODO

- Specify variables and their possible discrete values (continuous variables must be quantised)
- Store the results of running simulations, indexed by input

