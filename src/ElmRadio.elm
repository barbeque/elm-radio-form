import Browser
import Forms.Field as FF
import Forms.Form as F
import Forms.Validation as FV
import Forms.Validation.Result as FR
import Forms.Update as FU
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict

type Msg
    = FormUpdated (FU.Msg String)

type Transmission
    = Manual
    | Automatic

type alias Car =
    { brand : String
    , model : String
    , transmission : Transmission
    , year : Int
    }

type FormError
    = EmptyString
    | NotInt
    | TooOld

type alias Model =
    { car : Car
    , form : F.Form String FormError Car
    , formResults : FR.FormResult String FormError Car
    }

type alias Flags =
    { }

formFields : FF.Fields String
formFields =
    FF.fields
        [ ("brand", FF.input)
        , ("model", FF.input)
        , ("transmission", FF.input)
        , ("year", FF.input)
        ]

formValidator : FV.Validate String FormError Car
formValidator fields =
    FV.valid Car
        |> FV.required fields "brand" (FV.stringField <| FV.notEmpty EmptyString FV.success)
        |> FV.required fields "model" (FV.stringField <| FV.notEmpty EmptyString FV.success)
        |> FV.hardcoded Manual -- FIXME
        |> FV.required fields "year" (FV.stringField <| FV.int NotInt <|
            \year ->
                if year < 1900 then
                    FV.failure TooOld
                else
                    FV.success year
            )

view : Model -> Html Msg
view model =
    div []
        [ inputField model "Brand" "brand"
        , inputField model "Model" "model"
        , inputField model "Year" "year"
        ]

inputField : Model -> String -> String -> Html Msg
inputField model caption fieldName =
    div []
        [ label [] [ text (caption ++ ":") ] 
        , input
            [ placeholder caption
            , onInput (FU.stringFieldMsg FormUpdated fieldName)
            ]
            []
        , case model.formResults of
            -- clunky, is this right?
            FR.Invalid d ->
                case Dict.get fieldName d of
                    Just de ->
                        case de of
                            NotInt -> text "Not a number."
                            EmptyString -> text "Put something in."
                            TooOld -> text "Too old."
                    _ -> text "No errors"
            _ -> text "All OK"
        ]    

init : Flags -> (Model, Cmd Msg)
init flags =
    let
        form = F.form formFields formValidator
    in
        ({ car =
            { brand = ""
            , model = ""
            , transmission = Manual
            , year = 2018
            }
        , form = form
        , formResults = F.validate form -- this can't be the right way to do this
        }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FormUpdated formMsg ->
            let
                form = FU.updateForm formMsg model.form

                -- always run validate
                formResults = F.validate form
                
                newModel = { model | form = form, formResults = formResults }

                console =
                    Debug.log "" (F.validate newModel.form)
            in
                (newModel, Cmd.none)

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
