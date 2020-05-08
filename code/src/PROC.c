#include <stdio.h>	/* fprintf(), printf() */
#include <stdlib.h>	/* atoi() */
#include <stdint.h>	/* uint32_t */
#include <math.h>

#include "RegFile.h"
#include "Syscall.h"
#include "utils/heap.h"
#include "elf_reader/elf_reader.h"
void R_type(uint32_t instruction);
void J_type(uint32_t instruction);
void Branch_I(uint32_t instruction);
void Immediate_I(uint32_t instruction);
void LS_I(uint32_t instruction);
/*
* This variable will store the address
* of the next instruction to be fetched
* from the instruction memory.
*
* changed this to a globle vriable 
* we can change it in every function
*/
uint32_t ProgramCounter;

void R_type(uint32_t instruction){
   uint8_t rs = (instruction & 0x3E00000) >> 21;
   uint8_t rt = (instruction & 0x1F0000) >> 16;
   uint8_t rd = (instruction & 0xF800) >> 11;
   uint8_t shamt = (instruction & 0x7C0) >> 6;
   uint8_t funct = instruction & 0x3F;
	/* Extract different part of the R_type instructions */
	uint32_t u_rt = RegFile[rt];
	uint32_t u_rs = RegFile[rs];
	uint32_t u_rd = RegFile[rd];
	/* Define unsigned value for registers */
	uint8_t opcode;//for branch delay slot
	uint32_t NextInstruction;//for branch delay slot
	int64_t lrs=RegFile[rs];//long rs
	int64_t lrt=RegFile[rt];//long rt
	int64_t mul;
	uint64_t u_mul;
	uint64_t lu_rs=u_rs;//long unsigned rs
	uint64_t lu_rt=u_rt;//long unsigned rt
	int32_t quo,rem;//quotient and remainder
	uint32_t u_quo,u_rem;//quotient and remainder for unsigned division

	uint32_t tempPC = ProgramCounter;
	/*Define other useful registers*/
	if (funct == 12 || funct == 13){
		//syscall
		SyscallExe((uint32_t)RegFile[2]);
	}
	switch(funct){
		case 0://sll
			RegFile[rd]=RegFile[rt]<<shamt;
			break;
		case 2://srl
			RegFile[rd]=u_rt>>shamt;
			break;
		case 3://sra
			RegFile[rd]=RegFile[rt]>>shamt;
			break;
		case 4://sllv
			RegFile[rd]=RegFile[rt]<<RegFile[rs];
			break;
		case 6://srlv
			RegFile[rd]=u_rt>>RegFile[rs];
			break;
		case 7://srav
			RegFile[rd]=RegFile[rt]>>RegFile[rs];
			break;
		case 8://jr
			//branch delay slot
			NextInstruction = readWord((ProgramCounter+4),0);
			opcode = (NextInstruction & 0XFC000000)>>26;
			
			if (0 == opcode){
				R_type(NextInstruction);
			}
			else if (opcode == 2 || opcode ==3){
				J_type(NextInstruction);
				ProgramCounter = tempPC+4;
			}
			else if (opcode >=8 && opcode <=15){
				Immediate_I(NextInstruction);
			}
			else if (opcode >15 && opcode <=46){
				LS_I(NextInstruction);
			}
			else
				ProgramCounter+=4;
			
			//jump
			ProgramCounter=(uint32_t)RegFile[rs]-4;
			
			break;
		case 9://jalr
			//branch delay slot
			NextInstruction = readWord((ProgramCounter+4),0);
			opcode = (NextInstruction & 0XFC000000)>>26;

			if (0 == opcode){
				R_type(NextInstruction);
			}
			else if (opcode == 2 || opcode ==3){
				J_type(NextInstruction);
				ProgramCounter = tempPC+4;
			}
			else if (opcode >=8 && opcode <=15){
				Immediate_I(NextInstruction);
			}
			else if (opcode >15 && opcode <=46){
				LS_I(NextInstruction);
			}
			else
				ProgramCounter+=4;
			
			//link
			RegFile[rd]=ProgramCounter+4;
			//jump
			ProgramCounter=(uint32_t)RegFile[rs]-4;
			break;
		case 16://mfhi
			RegFile[rd]=RegFile[32];
			break;
		case 17://mthi
			RegFile[32]=RegFile[rs];
			break;
		case 18://mflo
			RegFile[rd]=RegFile[33];
			break;
		case 19://mtlo
			RegFile[33]=RegFile[rs];
			break;
		case 24://mult
			mul=lrs*lrt;
			RegFile[33]=(mul<<32)/pow(2,32);//move lower part to LO
			RegFile[32]=mul>>32;//move upper part to HI
			break;
		case 25://multu
			u_mul=lu_rs*lu_rt;
			RegFile[33]=u_mul & 0xFFFFFFFF;
			RegFile[32]=u_mul>>32;//move upper part to HI
			break;
		case 26://div
			quo=RegFile[rs]/RegFile[rt];
			rem=RegFile[rs]%RegFile[rt];
			RegFile[33]=quo;//move quotient to LO
			RegFile[32]=rem;//move remainder to HI
			break;
		case 27://divu
			u_quo=u_rs/u_rt;
			u_rem=u_rs%u_rt;
			RegFile[33]=u_quo;//move quotient to LO
			RegFile[32]=u_rem;//move remainder to HI
			break;
		case 32://add
			RegFile[rd]=RegFile[rs]+RegFile[rt];
			break;
		case 33://addu
			u_rd=u_rs+u_rt;
			RegFile[rd]=u_rd;
			break;
		case 34://sub
			RegFile[rd]=RegFile[rs]-RegFile[rt];
			break;
		case 35://subu
			u_rd=u_rs-u_rt;
			RegFile[rd]=u_rd;
			break;
		case 36://and
			RegFile[rd]=RegFile[rs]&RegFile[rt];
			break;
		case 37://or
			RegFile[rd]=RegFile[rs]|RegFile[rt];
			break;
		case 38://xor
			RegFile[rd]=RegFile[rs]^RegFile[rt];
			break;
		case 39://nor
			RegFile[rd]=~(RegFile[rs]|RegFile[rt]);
			break;
		case 42://slt
			if (RegFile[rs]<RegFile[rt])
				RegFile[rd]=1;
			else
				RegFile[rd]=0;
			break;
		case 43://sltu
			if (u_rs<u_rt)
				RegFile[rd]=1;
			else
				RegFile[rd]=0;
			break;
	}

	ProgramCounter+=4;
	RegFile[0]=0;
}

void J_type(uint32_t instruction){
	uint8_t opcode = (instruction & 0XFC000000)>>26;
	uint32_t offset = (instruction & 0x3FFFFFF);

	//branch delay slot
	uint32_t NextInstruction;
	NextInstruction = readWord((ProgramCounter+4),0);

	uint8_t opcode2 = (NextInstruction & 0XFC000000)>>26;
	uint32_t tempPC = ProgramCounter;
	
  uint32_t inst_index;
  uint32_t temp;

	if (0 == opcode2){
		R_type(NextInstruction);
	}
	else if (opcode2 == 2 || opcode2 ==3){
		J_type(NextInstruction);
		ProgramCounter = tempPC+4;
	}
	else if (opcode2 >=8 && opcode2 <=15){
		Immediate_I(NextInstruction);
	}
	else if (opcode2 >15 && opcode2 <=46){
		LS_I(NextInstruction);
	}
	else
		ProgramCounter+=4;

	//jump
	if (opcode == 2){

		temp=(ProgramCounter)& 0XF0000000;
		inst_index=temp+(offset<<2);
		ProgramCounter=inst_index;
	}
	//jal
	else if (opcode == 3){
    RegFile[31] = ProgramCounter + 4;
		
		temp=(ProgramCounter)& 0XF0000000;
		inst_index=temp+(offset<<2);
		ProgramCounter= inst_index;
	}
	RegFile[0]=0;
}

void Branch_I(uint32_t instruction){
	//decode the instruction
	uint8_t opcode = (instruction & 0XFC000000)>>26;
 	uint8_t rs = (instruction & 0x3E00000) >> 21;
	uint8_t rt = (instruction & 0x001F0000) >> 16;
	int16_t offset_1 = (instruction & 0x0000FFFF);
	int32_t offset = (int32_t)offset_1 <<2;

	int temp_rs = RegFile[rs];
	int temp_rt = RegFile[rt];
	//branch delay slot
	uint32_t DelayInstruction = readWord((ProgramCounter+ 4),false);
	uint8_t opcode2 = (DelayInstruction & 0XFC000000)>>26;
	uint32_t tempPC = ProgramCounter;
	if (0 == opcode2){
		R_type(DelayInstruction);
	}
	else if (opcode2 == 2 || opcode2 ==3){
		J_type(DelayInstruction);
		ProgramCounter = tempPC+4;
	}
	else if (opcode2 >=8 && opcode2 <=15){
		Immediate_I(DelayInstruction);
	}
	else if (opcode2 >15 && opcode2 <=46){
		LS_I(DelayInstruction);
	}
	else
		ProgramCounter+=4;
		
	
	//branch to the location 

	switch (opcode){
		case 1:
			switch(rt){
				case 0://BLTZ
				if (temp_rs < 0)
					ProgramCounter += offset;
				else ProgramCounter +=4;
				break;

				case 1://BGEZ
				if (temp_rs >= 0)
					ProgramCounter += offset;
				else ProgramCounter +=4;
				break;

				case 16://BLTZAL
				if (temp_rs < 0){
					RegFile[31] = ProgramCounter + 4;
					ProgramCounter += offset;
				}
				else ProgramCounter +=4;
				break;

				case 17://BGEZAL
				if (temp_rs >= 0){
					RegFile[31] = ProgramCounter + 4;
					ProgramCounter += offset;
				}
				else ProgramCounter +=4;
				break;
			}
			break;
		case 4://BEQ
			if (temp_rs == temp_rt)
				ProgramCounter += offset;
			else ProgramCounter +=4;
		break;

		case 5://BNE
			if (temp_rs != temp_rt)
				ProgramCounter += offset;
			else ProgramCounter +=4;
		break;

		case 6://BLEZ
			if (temp_rs <= 0)
				ProgramCounter += offset;
			else ProgramCounter +=4;
		break;

		case 7://BGTZ
			if (temp_rs > 0)
				ProgramCounter += offset;
			else ProgramCounter +=4;
		break;
		}
	RegFile[0] = 0;

}
void Immediate_I(uint32_t instruction){
	//decode the instruction
	uint8_t opcode = (instruction & 0XFC000000)>>26;
  	uint8_t rs = (instruction & 0x3E00000) >> 21;
	uint8_t rt= (instruction & 0x001F0000) >> 16;
	int16_t immediate = (instruction & 0x0000FFFF);
	int temp_rs = RegFile[rs];

	//operation
	switch (opcode){
		case 8: //ADDI
			RegFile[rt] = temp_rs +(int32_t)immediate;
		break;
		case 9://ADDIU
			RegFile[rt] = temp_rs + (int32_t)immediate;
		break;
		case 10://SLTI
			if (temp_rs < (int32_t)immediate)
			RegFile[rt] = 1;
			else{
				RegFile[rt] = 0;
			}
		break;
		case 11://SLTIU
			if (temp_rs < (uint32_t)(immediate))
				RegFile[rt] = 1;
			else{
				RegFile[rt] = 0;
			}
		break;
		case 12://ANDI
			RegFile[rt] = (uint16_t)immediate & temp_rs;
		break;
		case 13://ORI
			RegFile[rt] = (uint16_t)immediate | temp_rs;
		break;
		case 14://XORI
			RegFile[rt] = (uint16_t)immediate ^ temp_rs;
		break;
		case 15://LUI
			RegFile[rt] = (int32_t)immediate << 16;
		break;
	}
	ProgramCounter+=4;
	RegFile[0] = 0;

}
void LS_I(uint32_t instruction){
	uint8_t opcode = (instruction & 0XFC000000)>>26;
  	uint8_t rs = (instruction & 0x3E00000) >> 21;
	uint8_t rt= (instruction & 0x001F0000) >> 16;
	int16_t offset = (instruction & 0x0000FFFF);
	int addres = RegFile[rs] + (int32_t)offset;
	uint32_t address = addres;
	int rem = offset%4;
	uint32_t mem_mask,reg_mask;
	int mem_data,reg_data,data,datar,datal;
	switch (opcode){
		case 32://LB
			data = readByte(address, false);
			RegFile[rt] = data;
			break;
		case 33://LH
			data = readWord(address, false)>>16;
			RegFile[rt] = data;
			break;
		case 34://LWL
			address = RegFile[rs] + (offset - rem);
						switch (rem){
				case 0:
					reg_mask = 0;
					mem_mask = 0xffffffff;
				break;
				case 1:
					reg_mask = 0x000000ff;
					mem_mask = 0x00ffffff;
				break;
				case 2:
					reg_mask = 0x0000ffff;
					mem_mask = 0x0000ffff;
				break;
				case 3:
					reg_mask = 0x00ffffff;
					mem_mask = 0x000000ff;
				break;

			}	
			mem_data = readWord(address, false) & mem_mask;
			reg_data = RegFile[rt] & reg_mask;
			RegFile[rt] = reg_data + (mem_data<<(8*rem));
			break;
		case 35://LW
			data = readWord(address, false);
			RegFile[rt] = data;
			break;
		case 36://LBU
			address = RegFile[rs] + (uint32_t)offset;
			data = readByte(address, false);
			RegFile[rt] = (uint32_t)data;
			break;
		case 37://LHU
			address = RegFile[rs] + (uint32_t)offset;
			data = readWord(address, false)>>16;
			RegFile[rt] = (uint32_t)data;
			break;
		case 38://LWR
			address = RegFile[rs] + (offset - rem);
			switch (rem){
				case 3:
					reg_mask = 0;
					mem_mask = 0xffffffff;
				break;
				case 0:
					reg_mask = 0x000000ff;
					mem_mask = 0x00ffffff;
				break;
				case 1:
					mem_mask = 0x0000ffff;
					reg_mask = 0x0000ffff;
				break;
				case 2:
					reg_mask = 0x00ffffff;
					mem_mask = 0x000000ff;
				break;

			}
			mem_data = readWord(address, false) & ~mem_mask;
			reg_data = RegFile[rt] & ~reg_mask;
			RegFile[rt] = reg_data + (mem_data>>(8*(3-rem)));
			break;
		case 40://SB
			data = RegFile[rt];
			writeByte(address, (uint8_t)data, false);
			break;
		case 41://SH
			datal = RegFile[rt]<<16;
			datar = readWord(address+16,false) >>16;
			data = datal + datar;
			writeWord(address, (uint16_t)data,false);
			break;
		case 42://SWL
			address = RegFile[rs] + (offset - rem);
			switch (rem){
				case 0:
					mem_mask = 0;
					reg_mask = 0xffffffff;
				break;
				case 1:
					mem_mask = 0xff000000;
					reg_mask = 0xffffff00;
				break;
				case 2:
					mem_mask = 0xffff0000;
					reg_mask = 0xffff0000;
				break;
				case 3:
					mem_mask = 0xffffff00;
					reg_mask = 0xff000000;
				break;

			}
			
			mem_data = readWord(address, false) &  mem_mask;
			reg_data = RegFile[rt] & reg_mask;
			data = (reg_data>>(8*rem)) + mem_data;
			writeWord(address, data,false);
			break;
		case 43://SW
			data = RegFile[rt];
			writeWord(address, data, false);
			break;
		case 46://SWR
			address = RegFile[rs] + (offset - rem);
			switch (rem){
				case 3:
					mem_mask = 0;
					reg_mask = 0xffffffff;
				break;
				case 0:
					reg_mask = 0x000000ff;
					mem_mask = 0x00ffffff;
				break;
				case 1:
					reg_mask = 0x0000ffff;
					mem_mask = 0x0000ffff;
				break;
				case 2:
					reg_mask = 0x00ffffff;
					mem_mask = 0x000000ff;
				break;

			}			
			mem_data = readWord(address, false) & mem_mask;
			reg_data = RegFile[rt] & reg_mask;
			data = (reg_data<<(8*(3-rem))) + mem_data;
			writeWord(address, data,false);
			break;	}
	ProgramCounter+=4;
	RegFile[0] = 0;
}


int main(int argc, char * argv[]) {

	/*
	 * This variable will store the maximum
	 * number of instructions to run before
	 * forcibly terminating the program. It
	 * is set via a command line argument.
	 */
	uint32_t MaxInstructions;

	/*
	 * This variable will store the instruction
	 * once it is fetched from instruction memory.
	 */
	uint32_t CurrentInstruction;

	//IF THE USER HAS NOT SPECIFIED ENOUGH COMMAND LINE ARUGMENTS
	if(argc < 3){

		//PRINT ERROR AND TERMINATE
		fprintf(stderr, "ERROR: Input argument missing!\n");
		fprintf(stderr, "Expected: file-name, max-instructions\n");
		return -1;

	}

     	//CONVERT MAX INSTRUCTIONS FROM STRING TO INTEGER	
	MaxInstructions = atoi(argv[2]);	

	//Open file pointers & initialize Heap & Regsiters
	initHeap();
	initFDT();
	initRegFile(0);

	//LOAD ELF FILE INTO MEMORY AND STORE EXIT STATUS
	int status = LoadOSMemory(argv[1]);

	//IF LOADING FILE RETURNED NEGATIVE EXIT STATUS
	if(status < 0){ 
		
		//PRINT ERROR AND TERMINATE
		fprintf(stderr, "ERROR: Unable to open file at %s!\n", argv[1]);
		return status; 
	
	}

	printf("\n ----- BOOT Sequence ----- \n");
	printf("Initializing sp=0x%08x; gp=0x%08x; start=0x%08x\n", exec.GSP, exec.GP, exec.GPC_START);

	RegFile[28] = exec.GP;
	RegFile[29] = exec.GSP;
	RegFile[31] = exec.GPC_START;

	printRegFile();

	printf("\n ----- Execute Program ----- \n");
	printf("Max Instruction to run = %d \n",MaxInstructions);
	fflush(stdout);
	ProgramCounter = exec.GPC_START;
	
	/***************************/
	/* ADD YOUR VARIABLES HERE */
	/***************************/
	

	int i;
	for(i = 0; i < MaxInstructions; i++) {

		//FETCH THE INSTRUCTION AT 'ProgramCounter'		
		CurrentInstruction = readWord(ProgramCounter,0);
		printf("instruction:%x\n", CurrentInstruction);
		printf("program counter: %x\n",ProgramCounter);
		/*if (CurrentInstruction == 0)
			j+=1;
		else
		{
			j=0;
		}
		if (j == 5){
			
			break;
		}*/ 
		//PRINT CONTENTS OF THE REGISTER FILE	
		printRegFile();
		
		/********************************/
		/* ADD YOUR IMPLEMENTATION HERE */
		/********************************/
		uint8_t opcode = (CurrentInstruction & 0XFC000000)>>26;
		uint8_t funct = CurrentInstruction & 0x3F;
		if (CurrentInstruction ==0){
			printf("nop\n");
		}
		if (0 == opcode){
			R_type(CurrentInstruction);
		}
		else if (opcode == 2 || opcode ==3){
			J_type(CurrentInstruction);
		}
		else if (opcode == 1 || (opcode >=4 && opcode <=7)) {
			Branch_I(CurrentInstruction);
		}
		else if (opcode >=8 && opcode <=15){
			Immediate_I(CurrentInstruction);
		}
		else if (opcode >15 && opcode <=46){
			LS_I(CurrentInstruction);
		}


	}   

	//Close file pointers & free allocated Memory
	closeFDT();
	CleanUp();

	return 0;

}
