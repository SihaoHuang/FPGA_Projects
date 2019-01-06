//basic module with some key elements
module test (a, b, c, d);
    input a, b;
    output c, d;
    reg A, B;
    wire C, D;
endmodule

//notes on some syntax
and or nand nor xor xnor buf not
//numerical constants
123
'd123, 'h7B, 'b11_1011 //_ is ignored, decimal, hex, bin radix respectively
16'd5 //16-bit constant b'0000_0000_0000_0101
//the question mark: conditionals
condition ? value_if_true : value_if_false
//concatenation and buses
{4{b1[3:0]},16'h0000} // 32-bit bus with 4 copies of b1[3:0], 16 0's

//and gate
module and_gate (input_1, input_2, and_result);
    input input_1;
    input input_2;
    output and_result;

    wire and_temp; //extra wire added for pedagogical reasons

    assign and_temp = input_1 & input_2;
    asign and_result = and_temp;
endmodule

//using always for combinatorial processes
//same as the assign statement above
always @ (input_1 or input_2) //sensitivity list
    begin
      and_gate = input_1 & input_2; //NOTE - <= is for blocking assignmens, used for combinatorial logic (executed line by line)
    end

//using always for sequential processes
//instantiates a register (flip-flop)
always @ (posedge i_clock)
    begin
        and_gate <= input_1 & input_2; //NOTE - <= is for nonblocking assignments, used for sequential logic (executed in parallel)
    end

//let's write something to test propagation delays
//we need to wait until clock cycle 4 for test4 to become 1, due to hardware limitations
reg test1 = 1'b1;
reg test2 = 1'b0;
reg test3 = 1'b0;
reg test4 = 1'b0;
always @ (posedge i_clock)
    begin
        test2 <= test1;
        test3 <= test2;
        test4 <= test3;
    end

//first project
//LED blinker
//<switch_1, switch_2>, <0,0> = 100Hz, <0,1> = 50Hz, <1,0> = 10Hz, <1,1> = 1Hz
//enable switch is AND'ed at the end

module led_blinker (i_clock, i_enable, i_sw1, i_sw2, o_led);
    input i_clock;
    input i_enable;
    input i_sw1;
    input i_sw2;
    output o_led;

    //constant is inputClock/targetFreq * DutyCycle
    parameter count_100Hz = 125;
    parameter count_50Hz = 250;
    parameter count_10Hz = 1250;
    parameter count_1Hz = 12500;

    //the counters
    reg [31:0] r_count_100Hz = 0;
    reg [31:0] r_count_50Hz = 0;
    reg [31:0] r_count_10Hz = 0;
    reg [31:0] r_count_1Hz = 0;

    //used to toggle
    reg  r_toggle_100Hz = 1'b0;
    reg  r_toggle_50Hz = 1'b0;
    reg  r_toggle_10Hz = 1'b0;
    reg  r_toggle_1Hz = 1'b0;

    //one bit select signal
    reg r_LED_select;

    begin 

        //create counters for different frequencies
        always @ (posedge i_clock)
            begin 
                if (r_count_100Hz == count_100Hz - 1) //-1 since counter starts at 0
                    begin
                        r_toggle_100Hz <= !r_toggle_100Hz;
                        r_count_100Hz <= 0;
                    end
                else
                    r_count_100Hz <= r_count_100Hz + 1;
            end

        always @ (posedge i_clock)
            begin 
                if (r_count_50Hz == count_50Hz - 1) //-1 since counter starts at 0
                    begin
                        r_toggle_50Hz <= !r_toggle_50Hz;
                        r_count_50Hz <= 0;
                    end
                else
                    r_count_50Hz <= r_count_50Hz + 1;
            end

        always @ (posedge i_clock)
            begin 
                if (r_count_10Hz == count_10Hz - 1) //-1 since counter starts at 0
                    begin
                        r_toggle_10Hz <= !r_toggle_10Hz;
                        r_count_10Hz <= 0;
                    end
                else
                    r_count_10Hz <= r_count_10Hz + 1;
            end
        
        always @ (posedge i_clock)
            begin 
                if (r_count_1Hz == count_1Hz - 1) //-1 since counter starts at 0
                    begin
                        r_toggle_1Hz <= !r_toggle_1Hz;
                        r_count_1Hz <= 0;
                    end
                else
                    r_count_1Hz <= r_count_1Hz + 1;
            end

        //multiplexer for switch inputs
        always @ (*)
            begin 
                case ({i_sw1, i_sw2}) //{} concatenates the two bits
                    2'b11 : r_LED_select <= r_toggle_1Hz;
                    2'b10 : r_LED_select <= r_toggle_10Hz;
                    2'b01 : r_LED_select <= r_toggle_50Hz;
                    2'b00 : r_LED_select <= r_toggle_100Hz;
                endcase
            end
        
        //finally, connect the wires
        assign o_led - r_LED_select & i_enable;

    end 
endmodule 

//boolean operators can be bitwise, reduction, or logical

//2-to-1 multiplexer with dual-polarity outputs
module mux2(input, a, b, sel, output z, zbar);
    assign z = sel ? b : a;
    assign zbar = ~z;
endmodule 

//hierarchy of modules
module mux4(input d0, d1, d2, d3, input [1:0] sel, output z);
    wire z1, z2;
    mux2 m1(.sel(sel[0]),.a(d0),.b(d1),.z(z1));
    mux2 m2(.sel(sel[0]),.a(d2),.b(d3),.z(z2));
    mux2 m3(.sel(sel[1]),.a(z1),.b(z2),.z(z));
endmodule

//32 bit adder with carry-in and carry-out
module add32_carry (input[31:0] a, b, input cin, output [31:0] sum, output cout);
    assign {cout, sum} = a + b + cin
endmodule 

//parameterized modules, default width here is set at W=1
module mux2 #(parameter W=1) (input [W-1:0] a, b, input sel, output [W-1:0] z);
    assign z = sel ? b : a;
    assign zbar = ~z;
endmodule

//sequential behaviors (more powerful control structures) available inside always blocks
module mux4(input a, b, c, d, input [1:0] sel, output reg z, zbar);
    always @(*) begin
        if (sel == 2'b00) z = a;
        else if (sel == 2'b01) z = b;
        else if (sel == 2b'10) z = c;
        else if (sel == 2b'11) z = d;
        zbar = ~z;
    end
endmodule 

//ports are declared as wires by default
//nets appearing on the LHS of assignment statements inside always blcoks must be declared as reg
//we can also declare inputs and outputs as reg
output reg [15:0] result

//case statements
//to avoid the unintentional creation of state, make sure every possible path gets a value
//i.e. if 2b'11 isn't assigned, and that input is given, the synthesis has to somehow remember the value
module mux4(input a, b, c, d, input [1:0] sel, output reg z, zbar);
    always @(*) begin
        case (sel)
            2'b00: z = a;
            2'b01: z = b;
            2'b10: z = c;
            2'b11: z = d;
            default: z = 1'bx;
        endcase
        zbar = ~z;
    end
endmodule 

//other useful features
for, while, repeat, forever
//one-time-only initialization
initial
//compile-time only computations
generate, genvar
//system tasks for simulations
$finish(), $display(), etc 

//let's write an ALU!
module mux32two (input [31:0] i0, i1, input sel, output [31:0] out);
    assign out = sel ? i1 : i0;
endmodule 

module add32 (input [31:0] i0, i1, output [31:0] sum);
    assign sum = i0 + i1;
endmodule

module sub32 (input [31:0] i0, i1, output [31:0] diff);
    assign diff = i0 - i1;
endmodule

module mux32three (input [31:0] i0, i1, i2, input[1:0] sel, output reg [31:0] out);
    always @ (i0 or i1 or i2 or sel) begin
        case (sel)
            2'b00: out = i0;
            2'b01: out = i1;
            2'b10: out = i2;
            default: out = 32'bx;
        endcase
    end
endmodule 

module mul16 (input (15:0) i0, i1, output [31:0] prod);
    assign prod = i0 * i1; //note this is a magnitude multiplier; unsigned
endmodule 

module alu (input [31:0] a, b, input [2:0] f, output [31:0] r);
    wire [31:0] submux_out;
    wire [31:0] add_out, sub_out, mul_out;
    mux32two sub_mux(b, 32'd1, f[0], submux_out); //here we are using explicit declarations, so order of ports matter
    add32 this_adder(a, addmux_out, add_out); //if we used the . before, then it doesn't matter
    sub32 this_subtracter(a, submux_out, sub_out);
    mul16 this_multiplier(a[15:0], b[15:0], mul_out);
    mux32three output_mux(add_out, sub_out, mul_out, f[2:1], r)'
endmodule