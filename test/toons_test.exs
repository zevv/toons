defmodule ToonsTest do
  use ExUnit.Case
  doctest Toons

  test "greets the world" do
    assert Toons.hello() == :world
  end
end
