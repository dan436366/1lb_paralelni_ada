with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Main is

   Max_Threads : constant := 100;
   subtype Thread_Index is Integer range 1 .. Max_Threads;


   type Flags_Array is array (Thread_Index) of Boolean;


   protected type Safe_Output is
      procedure Print (Msg : String);
   end Safe_Output;

   protected body Safe_Output is
      procedure Print (Msg : String) is
      begin
         Ada.Text_IO.Put_Line (Msg);
      end Print;
   end Safe_Output;

   Console : Safe_Output;


   protected type Stop_Manager is
      procedure Set_Stop(ID : Integer);
      function Is_Stopped(ID : Integer) return Boolean;
   private
      Flags : Flags_Array := (others => False);
   end Stop_Manager;

   protected body Stop_Manager is
      procedure Set_Stop(ID : Integer) is
      begin
         Flags(ID) := True;
      end Set_Stop;

      function Is_Stopped(ID : Integer) return Boolean is
      begin
         return Flags(ID);
      end Is_Stopped;
   end Stop_Manager;

   Stop_Flags : Stop_Manager;

   task type Worker is
      entry Start (ID : Integer; Step : Integer);
   end Worker;

   task body Worker is
      My_ID    : Integer := 0;
      My_Step  : Integer := 1;
      Sum      : Long_Long_Integer := 0;
      Count    : Integer := 0;
      Current  : Integer := 0;
   begin
      accept Start (ID : Integer; Step : Integer) do
         My_ID := ID;
         My_Step := Step;
      end Start;

      loop
         exit when Stop_Flags.Is_Stopped(My_ID);
         Sum := Sum + Long_Long_Integer(Current);
         Current := Current + My_Step;
         Count := Count + 1;
         delay 0.05;
      end loop;

      Console.Print("Thread " & Integer'Image(My_ID) &
                    ": sum = " & Long_Long_Integer'Image(Sum) &
                    ", addends = " & Integer'Image(Count));
   end Worker;


   task type Stopper is
      entry Start (Target_ID : Integer; Delay_Secs : Duration);
   end Stopper;

   task body Stopper is
      ID : Integer := 0;
      Wait : Duration := 1.0;
   begin
      accept Start (Target_ID : Integer; Delay_Secs : Duration) do
         ID := Target_ID;
         Wait := Delay_Secs;
      end Start;

      delay Wait;
      Stop_Flags.Set_Stop(ID);
   end Stopper;


   Workers  : array (Thread_Index) of Worker;
   Stoppers : array (Thread_Index) of Stopper;

   Num_Threads : Integer;
   Step        : Integer;

begin
   Put("Enter number of threads: ");
   Get(Num_Threads);
   Put("Enter step of threads: ");
   Get(Step);

   if Num_Threads > Max_Threads then
      Put_Line("Too many threads.");
      return;
   end if;

   for I in 1 .. Num_Threads loop
      Workers(I).Start(I, Step);
      Stoppers(I).Start(I, Duration(I));
   end loop;
end Main;
