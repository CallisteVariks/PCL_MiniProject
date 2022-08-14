module University

type EPoints = { Five : int; Ten : int}

type Department = 
    | IT 
    | Mechanical 
    | SocialStudies 
    | Marketing 
    | Medicine
    | NoDep 

type Traits = 
    | Programming
    | Mathematics
    | Economics
    | Biology
    | None


type PTraits = {Traits : int} 

type NonGroupedStudent={name:string; age:int; EPoint:EPoints; PTraits:PTraits}

let Steven = {name="Steven"; age=22; EPoints.Five=5; Traits.Programming=8}

let Iva = {name="Iva"; age= 20; EPoints.Ten=10; Traits={name="Social";scaleP=7}}

let Kusky = {name="Kusky"; age=21; EPoints.Ten=10; PTraits={name="Innovative";scaleP=10}}

let Willy = {name="Willy"; age=24; EPoints.Five=5; PTraits={name="Devoted";scaleP=8}};

let John = {name="John"; age=22; EPoints.Five=5; PTraits={name="Kind";scaleP=8}}
 
 
let grouping (student : NonGroupedStudent):Department =
        let result:Department=
          match student.PTraits.name with
                | "Biology" when student.PTraits.scaleP >= 9 ->HealthCare 
                | "Programming" when student.PTraits.scaleP>= 7 ->IT
                | "Mathematics" when student.PTraits.scaleP >= 7 ->Mechanical
                | "Sociology" when student.PTraits.scaleP >= 7 ->SocialStudies
                | "Research" when student.PTraits.scaleP >= 7->Marketing
                | _ -> NoDep
        result;;
  
    
    //Test Phase 1
let test1 = grouping(Willy)
let test2= grouping(John)
let test3= grouping(Iva)
let test4= grouping(Steven)
let test5= grouping(Kusky)


//Phase 2


let ePoints =5,10

type RealStudent(Name:string, Age:int, ePoints, Department:string)=
    member x.Name = Name;
    member x.Age= Age;
    member x.EPoint = ePoints;
    member x.Department= Department;
    //110 minimum points

(*let register(student:NonGroupedStudent):RealStudent =
    let result =
       if student.EPoint>=110 then 
        //  let department =grouping(student);
        //  RealStudent(Name=student.Name,Age=student.Age,ePoints=student.EPoint,Department=department)
       else
         RealStudent(Name="",Age=0,ePoints=0,Department="")
    result;;
   

let f = register(Kusky).Department


type StudyMeritMessage(WaivePoint: int,LeaveMessage:string) =
    
    member x.WaivePoint = WaivePoint
    member x.LeaveMessage = LeaveMessage

    //110 minimum points

let grantMeritAgent= 
    MailboxProcessor<StudyMeritMessage>.Start(fun inbox-> 
        let rec msgLoop= async{
            let! msg= inbox.Receive() 
            let message = msg.LeaveMessage
            match (msg.WaivePoint,msg.LeaveMessage)with
                | (0,message)  when msg.LeaveMessage.Contains("party") -> printfn "Leave not granted. Go study"
                | (0,message) -> printfn "Leave  granted."
                | (_,"") when msg.WaivePoint>=110 -> printfn "Study merit granted. Job well done"
                | (_,"") when msg.WaivePoint<110 -> printfn "Study merit DENIED! GO STUDY!"
                | (0,"") ->printfn "Message format not percieved. Please try again!"
            return! msgLoop 
        }
        msgLoop)     

        //type 0  with a message if you wish to send a Leave Message
        //type a number with an empty string if you wish the ask for merit
grantMeritAgent.Post ((StudyMeritMessage(105,"")))
grantMeritAgent.Post ((StudyMeritMessage(110,"")))
grantMeritAgent.Post ((StudyMeritMessage(0,"I need to go to a party. Please let me leave")))
grantMeritAgent.Post ((StudyMeritMessage(0,"I need to go see my family and my cat.")))
grantMeritAgent.Post ((StudyMeritMessage(0,"")))


*)