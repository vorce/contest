defmodule Contest do
  use Supervisor.Behaviour

  def init(user_options) do
    tree = [ worker(ExUnit.Runner, [user_options]) ]
    supervise(tree, strategy: :one_for_one)
  end

  # { :ok, pid } = :supervisor.start_link(MyServer, [])
end
