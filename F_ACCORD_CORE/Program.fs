// importing some libraries here, using Accord.NET

open Accord
open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Linear
open Accord.Statistics.Models.Regression.Fitting
open FSharp.Data
open FSharp.Data.Runtime

// determining which predictors we are going to use for prediction

let predictors = 
    [|
        "yr";
        "mnth";
        "holiday";
        "weekday";
        "workingday";
        "weathersit";
        "temp";
        "atemp";
        "hum";
        "windspeed";
        "casual";
        "registered"
    |]
let printSourceLocation() =
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__
printSourceLocation()
// defining the type that will store the bike data
type BikeData = {Predictors: float[][]; Outputs: float[][]}

// defining the function that will load our dataset and divide it into predictors and outputs
//type Stocks = CsvProvider<"data/MSFT.csv", ResolutionFolder= __SOURCE_DIRECTORY__ >
let getBikeData () =
    //type start = CsvProvider<"../data/MSFT.csv">
    let bikes = CsvFile.Load(__SOURCE_DIRECTORY__ + "\\data\\day.csv").Cache()
    let inputs = 
        bikes.Rows 
        |> Seq.map (fun x -> 
                        predictors |> Array.map (fun a -> 
                                                    float(x.GetColumn a))) 
        |> Seq.toArray
    let outputs = 
        bikes.Rows 
        |> Seq.map (fun x -> [|float(x.GetColumn "cnt")|]) 
        |> Seq.toArray
    {Predictors = inputs; Outputs = outputs}

let bikeDemand () = 
    // getting the bike data for training
    let data = getBikeData ()
    // creation of the instance of Multiple Linear Regression with 12 predictors and 1 output
    let regression  = new MultivariateLinearRegression(12, 1)

    // learning phase
    // "Regress" function contains the logic for:
    // making the random guess, searching for the mistake and fixing the mistake
    let error = regression.Regress(data.Predictors, data.Outputs)

    // starting from here we define the set of predictors that we will use for testing
    let year = 2015.

    // ( 1 to 12)
    let month = 11.

    //  weather day is holiday or not 
    let holiday = 0.

    // if day is neither weekend nor holiday is 1, otherwise is 0
    let workingday = 0.

    // Day of the week
    let weekday = 1.

    // 1: Clear, Few clouds, Partly cloudy, Partly cloudy
    // 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
    // 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
    // 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
    let weathersit = 1.
    
    // Normalized temperature in Celsius. The values are divided to 41 (max)
    let temp = 12.849153

    // Normalized feeling temperature in Celsius. The values are divided to 50 (max)
    let atemp = 10.17435

    // Normalized humidity. The values are divided to 100 (max)
    let hum = 82.75

    // Normalized wind speed. The values are divided to 67 (max)
    let windspeed = 10.024682

    // Count of casual users
    let casual = 539.

    //	count of registered users
    let registered = 1586.
   
   // computing the output for our set of predictors.
   // we are using normalized values here, because the dataset has been in already normalized state.
    let y = regression.Compute([|
                                   year;
                                   month;
                                   holiday;
                                   workingday;
                                   weekday;
                                   weathersit;
                                   temp / 41.;
                                   atemp / 50.;
                                   hum / 100.;
                                   windspeed / 67.;
                                   casual;
                                   registered;
                                |])
    
    printfn "Predicting the demand on bike rentals = %A" (y.[0] |> int)
    
    // if we want to find out the value for weights we can use the Coefficients property of the regression
    let thetas = regression.Coefficients

    System.Console.ReadLine() |> ignore
    ()
    
bikeDemand()