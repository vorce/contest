# Contest

Continuous testing (ala infinitest) for Elixir

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

