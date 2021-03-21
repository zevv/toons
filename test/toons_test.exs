defmodule ToonsTest do
  use ExUnit.Case
  doctest Toons
  
  test "get_tag" do
    props = [{"class", "red"}]
    assert Toons.get_tag("flop", 0, props) == "flop"
    assert Toons.get_tag("flop", 1, props) == "flop1"
    props = [{"class", "red"}, {"id", "aap"}]
    assert Toons.get_tag("flop", 0, props) == "aap"
    assert Toons.get_tag("flop", 1, props) == "aap"
  end

end
