defmodule ContestDirWorker do
  use GenServer.Behaviour

  defrecord DirWatcher,
    absolute_path: nil,
    files: [],
    interval: 1000,
    regex: nil,
    processor_pids: []

  defrecord FileMod, file_name: nil, mtime: nil

  def populate_files(in_dir) do
    { result, files } = :file.list_dir(in_dir)

    # We only deal with regular files
    files = Enum.filter files, fn(f) ->
      if(File.regular?(f), do: f)
    end

    # Note. This can (will?) fail if files is empty :(
    { result, Enum.map files, create_filemod(&1) }
  end

  defp create_filemod(file_name) do
    { result, stat } = File.stat file_name

    FileMod.new(file_name: file_name,
                mtime: stat.mtime)
  end

  @doc "Init the contest worker to monitor a directory"
  def init({dir_path, interval}) do
    File.dir?(dir_path) do # TODO fix me, will not actually return here
      { :stop, dir_path <> " isn't actually a directory", DirWatcher.new() }
    end

    # fill a list of FileMods
    { result, file_mods } = populate_files(dir_path)

    { result,
      DirWatcher.new(absolute_path: Path.absname(dir_path),
                     files: file_mods, interval: interval),
      interval }
  end

  # return a list of untracked files and files where their mtime
  # is newer than the one we have saved
  defp file_delta(current_list, new_list) do
    new_filemods = Enum.filter new_list, fn(fm) ->
      not List.member?(current_list, fm)
    end

    Enum.map new_filemods, fn({FileMod, f, _}) ->
      f
    end
  end

  @doc "This is where the action is"
  def handle_info(:timeout, config) do
    # populate new files
    { result, file_mods } = populate_files(config.absolute_path)
    #IO.puts "Number of files in dir right now: " <> integer_to_binary(Enum.count file_mods)

    # compare to current ones
    delta = file_delta(config.files, file_mods)
    #IO.puts "Got a bunch of stuff to process: " <> inspect delta

    config = config.files(file_mods)

    # send changed ones off to processing
    # Enum.map delta, do_processing(Path.absname(&1))
    Enum.map delta, fn(f) ->
      if f != nil and f != "" do
        do_processing(Path.absname(f), config.processor_pids)
      end
    end

    { :noreply, config, config.interval }
  end

  @doc "Sends the file for processing if it is in our config.files"
  def handle_cast({:process, file_name}, config) do
    fm = get_filemod_with_filename(config.files, file_name)

    if fm != nil do
      #IO.puts "Ok will process: " <> inspect fm
      do_processing(Path.absname(fm.file_name), config.processor_pids)
    end

    { :noreply, config, config.interval }
  end

  defp get_filemod_with_filename(file_mods, file_name) do
    Enum.find file_mods, fn({FileMod, f, _}) ->
      f == file_name
    end
  end

  def do_processing(of_file, with) do
    # TODO send message (containing of_file)
    # to another process that will actually
    # do something
    IO.puts "Hello. I am asked to process " <> (inspect of_file) <> ", but I have no power to do so. Sorry!"

    Enum.map with, :gen_server.cast(&1, {:process, of_file})
  end

  def handle_cast(:stop, config) do
      { :stop, "Stopped", config }
  end

  # { :ok, pid } = :gen_server.start_link(ContestDirWorker, {".", 5000}, [])
  # :gen_server.cast(pid, :stop)
  # :gen_server.cast(pid, {:process, "README.md"})
end

