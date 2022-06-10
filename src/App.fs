module App

open System
open Elmish
open Elmish.React
open Feliz

type Todo =
    { Id: Guid
      Description: string
      Completed: bool }

type TodoBeignEdited =
    { Id: Guid
      Description: string
      Edited: bool }

type ViewTodo =
    | All
    | Completed
    | NotCompleted

type State =
    { TodoList: Todo list
      NewTodo: string
      TodoBeingEdited: TodoBeignEdited option
      CurrentViewTodo: ViewTodo }

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string
    | SetCurrentViewTodo of ViewTodo

let init () =
    { TodoList =
        [ { Id = Guid.NewGuid()
            Description = "Learn F#"
            Completed = true }
          { Id = Guid.NewGuid()
            Description = "Learn Elmish"
            Completed = false } ]
      NewTodo = ""
      TodoBeingEdited = None
      CurrentViewTodo = All }

let update (msg: Msg) (state: State) : State =
    match msg with
    | SetNewTodo todoText -> { state with NewTodo = todoText }

    | DeleteTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

    | AddNewTodo when state.NewTodo = "" -> state

    | AddNewTodo ->
        let nextTodo =
            { Id = Guid.NewGuid()
              Description = state.NewTodo
              Completed = false }

        { state with
            NewTodo = ""
            TodoList = List.append state.TodoList [ nextTodo ] }

    | ToggleCompleted todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoId then
                    { todo with Completed = not todo.Completed }
                else
                    todo)

        { state with TodoList = nextTodoList }

    | StartEditingTodo todoId ->
        let nextEditMode =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map (fun todo ->
                { Id = todoId
                  Description = todo.Description
                  Edited = false })

        { state with TodoBeingEdited = nextEditMode }

    | CancelEdit -> { state with TodoBeingEdited = None }

    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
                state.TodoList
                |> List.map (fun todo ->
                    if todo.Id = todoBeingEdited.Id then
                        { todo with Description = todoBeingEdited.Description }
                    else
                        todo)

            { state with
                TodoList = nextTodoList
                TodoBeingEdited = None }

    | SetEditedDescription newText ->
        let nextEditMode =
            state.TodoBeingEdited
            |> Option.map (fun todoBeingEdited ->
                { todoBeingEdited with
                    Description = newText
                    Edited = true })

        { state with TodoBeingEdited = nextEditMode }

    | SetCurrentViewTodo todoView -> { state with CurrentViewTodo = todoView }

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [ prop.classes classes
               prop.children children ]

let appTitle =
    Html.p [ prop.className "title"
             prop.text "Elmish To-Do List" ]

let inputField (state: State) (dispatch: Msg -> unit) =
    div [ Bulma.Field; Bulma.HasAddons ] [
        div [ Bulma.Control; Bulma.IsExpanded ] [
            Html.input [ prop.classes [ Bulma.Input; Bulma.IsMedium ]
                         prop.valueOrDefault state.NewTodo
                         prop.onChange (SetNewTodo >> dispatch) ]
        ]

        div [ "control" ] [
            Html.button [ prop.classes [ Bulma.Button
                                         Bulma.IsPrimary
                                         Bulma.IsMedium ]
                          prop.onClick (fun _ -> dispatch AddNewTodo)
                          prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaPlus ] ] ] ]
        ]
    ]

let renderEditForm (todoBeingEdited: TodoBeignEdited) (dispatch: Msg -> unit) =
    div [ Bulma.Box ] [
        div [ Bulma.Field; Bulma.IsGrouped ] [
            div [ Bulma.Control; Bulma.IsExpanded ] [
                Html.input [ prop.classes [ Bulma.Input; Bulma.IsMedium ]
                             prop.valueOrDefault todoBeingEdited.Description
                             prop.onTextChange (SetEditedDescription >> dispatch) ]
            ]

            div [ Bulma.Control; Bulma.Buttons ] [
                Html.button [ prop.classes [ Bulma.Button 
                                             if todoBeingEdited.Edited then
                                                 Bulma.IsPrimary 
                                             else
                                                 Bulma.IsOutlined ]
                              prop.onClick (fun _ -> dispatch ApplyEdit)
                              prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaSave ] ] ] ]

                Html.button [ prop.classes [ Bulma.Button; Bulma.IsWarning ]
                              prop.onClick (fun _ -> dispatch CancelEdit)
                              prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaArrowRight ] ] ] ]
            ]
        ]
    ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    div [ Bulma.Box ] [
        div [ Bulma.Columns
              Bulma.IsMobile
              Bulma.IsVcentered ] [
            div [ Bulma.Column ] [
                Html.p [ prop.className Bulma.Subtitle 
                         prop.text todo.Description ]
            ]

            div [ Bulma.Column; Bulma.IsNarrow ] [
                div [ Bulma.Buttons ] [
                    Html.button [ prop.classes [ Bulma.Button 
                                                 if todo.Completed then Bulma.IsSuccess ]
                                  prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ] ] ]

                    Html.button [ prop.classes [ Bulma.Button; Bulma.IsPrimary ]
                                  prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaEdit ] ] ] ]

                    Html.button [ prop.classes [ Bulma.Button; Bulma.IsDanger ]
                                  prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                                  prop.children [ Html.i [ prop.classes [ FA.Fa; FA.FaTimes ] ] ] ]
                ]
            ]
        ]
    ]

let todoList (state: State) (dispatch: Msg -> unit) =
    let todoList =
        match state.CurrentViewTodo with
        | All -> state.TodoList
        | Completed ->
            state.TodoList
            |> List.filter (fun i -> i.Completed)
        | NotCompleted ->
            state.TodoList
            |> List.filter (fun i -> i.Completed <> true)

    Html.ul [ prop.children [ for todo in todoList ->
                                  match state.TodoBeingEdited with
                                  | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
                                      renderEditForm todoBeingEdited dispatch
                                  | _ -> renderTodo todo dispatch ] ]

let rec renderFilterTabs (state: State) (dispatch: Msg -> unit) =
    div [ Bulma.Tabs; Bulma.IsToggle; Bulma.IsFullwidth ] [
        Html.ul [ Html.li [ prop.className [ if state.CurrentViewTodo = All then
                                                 Bulma.IsActive ]
                            prop.children [ Html.a [ prop.onClick (fun _ -> dispatch (SetCurrentViewTodo All))
                                                     prop.text "All" ] ] ]
                  Html.li [ prop.className [ if state.CurrentViewTodo = Completed then
                                                 Bulma.IsActive ]
                            prop.children [ Html.a [ prop.onClick (fun _ -> dispatch (SetCurrentViewTodo Completed))
                                                     prop.text "Completed" ] ] ]
                  Html.li [ prop.className [ if state.CurrentViewTodo = NotCompleted then
                                                 Bulma.IsActive ]
                            prop.children [ Html.a [ prop.onClick (fun _ -> dispatch (SetCurrentViewTodo NotCompleted))
                                                     prop.text "Not Completed" ] ] ] ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [ prop.style [ style.padding 20 ]
               prop.children [ appTitle
                               inputField state dispatch
                               renderFilterTabs state dispatch
                               todoList state dispatch ] ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
