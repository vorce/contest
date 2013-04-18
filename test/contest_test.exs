Code.require_file "../test_helper.exs", __FILE__

#ExUnit.start
defmodule ContestTest do
  use ExUnit.Case, async: false

  Code.require_file "lib/contest_worker.ex"

  test "ContestDirWorker.populate_files should not list directories" do
    { :ok, files } = ContestDirWorker.populate_files(".")

    dirs = Enum.filter files, fn({ContestDirWorker.FileMod, f, _}) ->
      File.dir?(f)
    end

    assert dirs == []
  end
end
