with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
procedure Main is
   subtype Step_Type is Positive range 1 .. 5;
   subtype Delay_Type is Positive range 1 .. 5;
   type Integer_Array is array (Positive range <>) of Integer;
   task type Sum_Thread(Id : Positive; Step : Step_Type) is
      entry Stop;
   end Sum_Thread;
   type Sum_Thread_Ptr is access Sum_Thread;
   type Sum_Thread_Ptr_Array is array (Positive range <>) of Sum_Thread_Ptr;
   procedure Free is new Ada.Unchecked_Deallocation(Sum_Thread, Sum_Thread_Ptr);
   task body Sum_Thread is
      Sum   : Long_Long_Integer := 0;
      Count : Long_Long_Integer := 0;
      Val   : Long_Long_Integer := 0;
      Running : Boolean := True;
   begin
      while Running loop
         select
            accept Stop do
               Running := False;
            end Stop;
         else
            Sum := Sum + Val;
            Val := Val + Long_Long_Integer(Step);
            Count := Count + 1;
         end select;
      end loop;
      Ada.Text_IO.Put_Line("Thread #" & Id'Image & " sum = " & Long_Long_Integer'Image(Sum) & ", addends = " & Long_Long_Integer'Image(Count));
   end Sum_Thread;
   -- Random Generators
   package Step_Random is new Ada.Numerics.Discrete_Random(Step_Type);
   package Delay_Random is new Ada.Numerics.Discrete_Random(Delay_Type);
   G_Step  : Step_Random.Generator;
   G_Delay : Delay_Random.Generator;
   function Millis_Since(Start : Ada.Real_Time.Time) return Long_Integer is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      use Ada.Real_Time;
   begin
      return Long_Integer(To_Duration(Now - Start) * 1000.0);
   end Millis_Since;
   procedure Controller(Threads : in out Sum_Thread_Ptr_Array; Delays : in Integer_Array) is
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Stopped    : array(1 .. Threads'Length) of Boolean := (others => False);
   begin
      loop
         declare
            Current_Millis : constant Long_Integer := Millis_Since(Start_Time);
            All_Stopped    : Boolean := True;
         begin
            for I in Threads'Range loop
               if not Stopped(I) then
                  All_Stopped := False;
                  if Current_Millis >= Long_Integer(Delays(I)) then
                     Threads(I).Stop;
                     Stopped(I) := True;
                  end if;
               end if;
            end loop;
            exit when All_Stopped;
         end;
      end loop;
   end Controller;
   -- Arrays and input
   Thread_Count : Integer;
begin
   Ada.Text_IO.Put("Enter number of threads: ");
   Ada.Integer_Text_IO.Get(Thread_Count);
   declare
      Steps   : Integer_Array(1 .. Thread_Count);
      Delays  : Integer_Array(1 .. Thread_Count);
      Threads : Sum_Thread_Ptr_Array(1 .. Thread_Count);
   begin
      Step_Random.Reset(G_Step);
      Delay_Random.Reset(G_Delay);
      for I in 1 .. Thread_Count loop
         Steps(I)  := Step_Random.Random(G_Step);
         Delays(I) := Delay_Random.Random(G_Delay) * 1000;
      end loop;
      for I in 1 .. Thread_Count loop
         Threads(I) := new Sum_Thread(Id => I, Step => Step_Type(Steps(I)));
      end loop;
      Controller(Threads, Delays);
      for I in 1 .. Thread_Count loop
         Free(Threads(I));
      end loop;
   end;
end Main;
