defmodule ElmMaru do
end

defmodule ElmMaru.Router do
end

defmodule ElmMaru.Router.Homepage do
  use Maru.Router

  get "/api" do
    header("Access-Control-Allow-Origin", "*")

    {:ok, binary} = File.read("lib/api-response.json")
    result = binary |> Poison.decode!

    conn |> json(result)
  end

  post "/api" do
    if Mix.env == :dev do
      :timer.sleep(2000) # just to fake a slow connection
    end

    header("Access-Control-Allow-Origin", "*")
    IO.inspect conn.params
    conn |> json(conn.params)
  end
end

defmodule ElmMaru.API do
  use Maru.Router

  mount ElmMaru.Router.Homepage

  rescue_from :all do
    text conn, "Elm endpoint"
  end
end
