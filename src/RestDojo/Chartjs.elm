port module RestDojo.Chartjs exposing (..)


type alias ChartLabel =
    String


type alias ChartData =
    Int


type alias ChartDataSet =
    { label : ChartLabel
    , data : List ChartData
    , borderColor : String
    }


type alias ChartInput =
    { labels : List ChartLabel
    , datasets : List ChartDataSet
    }
