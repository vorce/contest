# Contest

Contest is continuous testing (ala infinitest) for Elixir

Work in progress. Learning Elixir project.

Note: This has apparently already been done: https://github.com/ElixirCasts/apprentice

## Design

### DirWorker

Monitors the files in a specific directory. Will send changed (or new) files
for processing.

Improvements: Take a regex in init. Only check files matching it.

### Processors

Performs the processing of new events/file changes

#### TestProcessor

Will run tests when triggered

### Contest

Controlls the DirWorkers and Processors

