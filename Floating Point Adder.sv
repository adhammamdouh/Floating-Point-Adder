module addMantissa
(
	input logic ALessThanB,
	input logic [23:0] mantissaA, mantissaB, shiftedMantissa,
	input logic [7:0] preExponent,
	input logic signA, signB,
    	input logic add,
	output logic [7:0] exponent,
	output logic [22:0] fraction,
    	output logic sign
);

logic [24:0] addtionResult, SubResult;
logic [23:0] addValue;
 
assign addValue = ALessThanB ? mantissaB[23:0] : mantissaA[23:0];

always_comb
    begin	
	if(add) begin
		addtionResult = {1'b0,addValue[23:0]} + {1'b0,shiftedMantissa[23:0]};
		
		sign = preExponent === 8'b11111111 ? ALessThanB ? signB : signA : mantissaB[23:0] > mantissaA[23:0] ? signB : signA;
		fraction = (addtionResult[24] ? addtionResult[23:1] : addtionResult[22:0]);
		exponent = addtionResult[24] ? (preExponent + 1) : (preExponent);
    	end
    end
endmodule 

//-----------------------------------------------------------------------------------------------------

module Submantissa(
	input logic ALessThanB,
	input logic [23:0] mantissaA, mantissaB, shiftedMantissa,
	input logic [7:0] preExponent,
	input logic signA, signB,
    	input logic sub,
	output logic [7:0] exponent,
	output logic [22:0] fraction,
    	output logic sign
);

logic [24:0] SubResult;
logic [24:0] shiftResult;
logic [7:0] exp;
logic [23:0] SubValue;

assign SubValue = ALessThanB ? mantissaB[23:0] : mantissaA[23:0];
assign sign = ALessThanB ? (SubValue[23:0] > shiftedMantissa[23:0] ? signB : signA) : (SubValue[23:0] > shiftedMantissa[23:0] ? signA : signB);
always_comb
	begin
		if(sub) begin
			if(shiftedMantissa[23:0] > SubValue[23:0])
				SubResult = {1'b0,shiftedMantissa[23:0]} - {1'b0,SubValue[23:0]};
			else 
				SubResult = {1'b0,SubValue[23:0]} - {1'b0,shiftedMantissa[23:0]};
			
			for(int i = 0 ; i <= 24 ; ++i) begin
				if(preExponent[7:0] === 8'b11111111 | preExponent[7:0] === 8'b00000000) break;
					shiftResult = SubResult[24:0] << (1'b1 * i);
					exp = preExponent - (1'b1 * i);		
					if((shiftResult[23] & 1'b1) | (shiftResult[24] & 1'b1)) break;
			end	
			if(preExponent[7:0] === 8'b11111111 | preExponent[7:0] === 8'b00000000) begin
				fraction = SubResult[22:0];
				exponent = preExponent;
			end
			else begin
				fraction = shiftResult[23:0];
				exponent = exp;
			end

			if(preExponent[7:0] !== 8'b11111111 & {1'b0,shiftedMantissa[23:0]} === {1'b0,SubValue[23:0]}) begin
   	 			exponent = 8'b0; 
   			end
		end
	end

endmodule  

//-----------------------------------------------------------------------------------------------------

module exponentCompare
(
    input logic [7:0] exponentA, exponentB,
    output logic ALessThanB,
    output logic [7:0] exponent, shiftMantissa
);

logic [7:0] AMinusB, BMinusA;  

assign AMinusB = exponentA - exponentB;
assign BMinusA = exponentB - exponentA;	
assign ALessThanB = exponentA[7:0] <= exponentB[7:0];

always_comb
    begin
        if(ALessThanB)
            begin
                exponent = exponentB;
		shiftMantissa = BMinusA;
            end
        else
            begin
                exponent = exponentA;
                shiftMantissa = AMinusB;
            end
    end

endmodule

//-----------------------------------------------------------------------------------------------------

module shiftMantissa
(
    input logic ALessThanB,
    input logic [23:0] mantissaA, mantissaB,
    input logic [7:0] shiftMantissa,
    output logic [23:0] shiftedMantissa,
    output logic [7:0] biggerExponent
);

logic [23:0] shiftedValue;

assign shiftedValue = ALessThanB ? (mantissaA >> shiftMantissa) : (mantissaB >> shiftMantissa);

always_comb
    begin        
	if(shiftMantissa[7] | shiftMantissa[6] | shiftMantissa[5] | (shiftMantissa[4] & shiftMantissa[3]))
            shiftedMantissa = 24'b0;
	if(ALessThanB & biggerExponent === 8'b1 & mantissaB === 24'b0)
	    shiftedMantissa = 24'b0;
	else if(~ALessThanB & biggerExponent === 8'b1 & mantissaA === 24'b0)
	    shiftedMantissa = 24'b0;
	else if(ALessThanB & biggerExponent === 8'b1 & mantissaB !== 24'b0)
	    shiftedMantissa = 24'b0;
	else if(~ALessThanB & biggerExponent === 8'b1 & mantissaA !== 24'b0)
	    shiftedMantissa = 24'b0;
        else
            shiftedMantissa = shiftedValue;
    end
endmodule

//-----------------------------------------------------------------------------------------------------

module SpecialCases
(
	input logic [23:0] mantissaA, mantissaB,
	input logic [7:0] exponentA, exponentB,
	input logic signA, signB,
	output logic special,
	output logic [7:0] exponent,
	output logic [22:0] fraction,
    	output logic sign
);
	always_comb
	if(exponentA === 8'b11111111 & mantissaA !== 23'b0) begin
		exponent = exponentA;
		fraction = mantissaA[22:0];
		sign = signA;
		special = 1'b1;
	end
	else if(exponentB === 8'b11111111 & mantissaB !== 23'b0)begin
		exponent = exponentB;
		fraction = mantissaB[22:0];
		sign = signB;
		special = 1'b1;
	end
	else if(exponentA === 8'b11111111 & mantissaA === 23'b0 & exponentB === 8'b11111111 & mantissaB === 23'b0 & signA !== signB) begin
		exponent = exponentB;
		fraction = {1'b1, mantissaB[21:0]};
		sign = signA;
		special = 1'b1;
	end
	else if(exponentA === 8'b11111111 & mantissaA === 23'b0)begin
		exponent = exponentA;
		fraction = mantissaA[22:0];
		sign = signA;
		special = 1'b1;
	end
	else if(exponentB === 8'b11111111 & mantissaB === 23'b0)begin
		exponent = exponentB;
		fraction = mantissaB[22:0];
		sign = signB;
		special = 1'b1;
	end
	else if(exponentA === 0 & mantissaA === 0)begin
		exponent = exponentB;
		fraction = mantissaB[22:0];
		sign = signB;
		special = 1'b1;
	end
	else if(exponentB === 0 & mantissaB === 0)begin
		exponent = exponentA;
		fraction = mantissaA[22:0];
		sign = signA;
		special = 1'b1;
	end
	else
		special = 1'b0;
endmodule

//-----------------------------------------------------------------------------------------------------

module floatingPointAdder
(
    input logic [31:0]a, b,
    output logic [31:0]s
);

logic [7:0] exponentA, exponentB, preExponent, exponent, shiftMantissa;
logic ALessThanB , sign;
logic [23:0] mantissaA, mantissaB, shiftedMantissa;
logic [22:0] fraction;
logic add, sub, special;
assign exponentA = a[30:23];
assign exponentB = b[30:23];

SpecialCases SC(mantissaA, mantissaB, exponentA, exponentB, a[31], b[31], special, exponent, fraction, sign);

always_comb
    begin
	if(exponentA[7:0] !== 8'b11111111 & exponentA[7:0] !== 8'b00000000)
		mantissaA = {1'b1 , a[22:0]};
	else
		mantissaA = {1'b0 , a[22:0]};
	if(exponentB[7:0] !== 8'b11111111 & exponentB[7:0] !== 8'b00000000)
		mantissaB = {1'b1 , b[22:0]};
	else
		mantissaB = {1'b0 , a[22:0]};
	if(~special) begin
		add = a[31] === b[31];
		sub = a[31] !== b[31];
	end
	else begin
		add = 1'b0;
		sub = 1'b0;
	end
    end

exponentCompare compare(exponentA, exponentB, ALessThanB, preExponent, shiftMantissa);
shiftMantissa shift(ALessThanB, mantissaA, mantissaB, shiftMantissa, shiftedMantissa, preExponent);
addMantissa addmant(ALessThanB, mantissaA, mantissaB, shiftedMantissa, preExponent, a[31], b[31], add, exponent, fraction, sign);
Submantissa submant(ALessThanB, mantissaA, mantissaB, shiftedMantissa, preExponent, a[31], b[31], sub, exponent, fraction, sign);    

	 
assign s = {sign, exponent, fraction};

endmodule
