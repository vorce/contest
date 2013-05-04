defmodule ContestElixirTestProcessor do
  use GenServer.Behaviour
  def handle_cast(:stop, config) do
      { :stop, "Stopped", config }
  end

  @doc "Run mix test"
  def handle_cast({:process, full_path_to_file}, config) do
  IO.puts "Im in da process exec shiz"

    if File.exists? full_path_to_file do
      IO.puts "Ok the file exists.. nicee!"

      samedir = Path.dirname(full_path_to_file)
      oneupdir = Path.expand(samedir <> "/..")

      { _, files } = :file.list_dir samedir
      if List.member? files, 'mix.exs' do
        IO.puts "Same dir as file has an mix.exs.. k"
        run_project_tests(samedir, "test")
      else
        { _, files } = :file.list_dir oneupdir
        if List.member? files, 'mix.exs' do
          IO.puts "One dir up from the file has an mix.exs orly"
          run_project_tests(oneupdir, "test")
        end
      end
    end
    { :noreply, config }
  end

  def run_tests() do
    { async, sync } = ExUnit.Server.cases
    ExUnit.Runner.run async, sync, ExUnit.Server.options
  end

  def load_tests(project_dir, test_dir) do
    File.cd(project_dir)

    Mix.loadpaths
    Mix.Task.run "loadpaths", ["--no-check"]
    Mix.Task.reenable "loadpaths"
    Mix.Task.reenable "deps.loadpaths"

    Mix.env(:test)
    Mix.Project.refresh
    Mix.Task.run Mix.project[:prepare_task]

    project = Mix.project

    test_helper = Keyword.get(project, :test_helper, test_dir <> "/test_helper.exs")
    Code.require_file(test_helper)

    test_paths = project[:test_paths] || [test_dir]
    test_pattern = project[:test_pattern] || "*_test.exs"
    files = Mix.Utils.extract_files(test_paths, test_pattern)

    Kernel.ParallelRequire.files files
  end

  def run_project_tests(project_dir, test_dir) do
    load_tests(project_dir, test_dir)
    run_tests()
    Mix.Task.reenable "test"
  end

  # { :ok, pid } = :gen_server.start_link(ContestElixirTestProcessor, nil, [])
  # :gen_server.cast(pid, :stop)
  # :gen_server.cast(pid, {:process, "/home/vorce/code/contest/README.md"})
end

defmodule ContestPrintProcessor do
  use GenServer.Behaviour

  def handle_cast(:stop, config) do
      { :stop, "Stopped", config }
  end

  @doc "Print that we were told to process this file"
  def handle_cast({:process, full_path_to_file}, config) do
    IO.puts (inspect ContestPrintProcessor) <> ": Received process command for: " <> (inspect full_path_to_file)

    if File.exists? full_path_to_file do
      IO.puts "Info: " <> (inspect File.stat(full_path_to_file))
    end
    { :noreply, config }
  end

  # { :ok, pid } = :gen_server.start_link(ContestPrintProcessor, {".", 5000}, [])
  # :gen_server.cast(pid, :stop)
  # :gen_server.cast(pid, {:process, "README.md"})
end
