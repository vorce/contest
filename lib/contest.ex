defmodule Contest do
  #use Supervisor.Behaviour

  #def init(user_options) do
  #  tree = [ worker(ExUnit.Runner, [user_options]) ]
  #  supervise(tree, strategy: :one_for_one)
  #end

  # { :ok, pid } = :supervisor.start_link(MyServer, [])
  def start(test_dir) do
    #File.cd(dir)
    Mix.loadpaths()
    Mix.env(:test)
    Mix.Project.refresh
    Mix.Task.run Mix.project[:prepare_task]

    project = Mix.project

    test_helper = Keyword.get(project, :test_helper, test_dir <> "/test_helper.exs")
    Code.require_file(test_helper)

    test_paths = project[:test_paths]  || [test_dir]
    test_pattern = project[:test_pattern] || "*_test.exs"
    files = Mix.Utils.extract_files(test_paths, test_pattern)

    Kernel.ParallelRequire.files files
    ExUnit.run
  end
end
