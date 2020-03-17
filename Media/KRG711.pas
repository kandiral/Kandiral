(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRG711                                                                    *)
(*  Ver.: 25.12.2016                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRG711;

interface

uses lgop;

function KRPCM2ALaw(pcm_val: smallint):byte;//	/* 2's complement (16-bit range) */
function KRALaw2PCM(a_val: byte): smallint;

function KRPCM2ULaw(pcm_val: smallint):byte;//	/* 2's complement (16-bit range) */
function KRULaw2PCM(u_val: byte): smallint;

function KRALaw2ULaw(aval:byte):byte;
function KRULaw2ALaw(uval:byte):byte;

implementation

const
  BIAS		= $84;//		/* Bias for linear code. */
  CLIP           = 8159;

  SIGN_BIT	= $80;//		/* Sign bit for a A-law byte. */
  QUANT_MASK	= $f;//		/* Quantization field mask. */
  NSEGS		= 8;//		/* Number of A-law segments. */
  SEG_SHIFT	= 4;//		/* Left shift for segment number. */
  SEG_MASK	= $70;//		/* Segment field mask. */

  seg_aend : array[0..7] of smallint = ($1F, $3F, $7F, $FF,
			    $1FF, $3FF, $7FF, $FFF);
  seg_uend : array[0..7] of smallint = ($3F, $7F, $FF, $1FF,
			    $3FF, $7FF, $FFF, $1FFF);

// copy from CCITT G.711 specifications */
  _u2a : array[0..127] of byte = (//			/* u- to A-law conversions */
	1,	1,	2,	2,	3,	3,	4,	4,
	5,	5,	6,	6,	7,	7,	8,	8,
	9,	10,	11,	12,	13,	14,	15,	16,
	17,	18,	19,	20,	21,	22,	23,	24,
	25,	27,	29,	31,	33,	34,	35,	36,
	37,	38,	39,	40,	41,	42,	43,	44,
	46,	48,	49,	50,	51,	52,	53,	54,
	55,	56,	57,	58,	59,	60,	61,	62,
	64,	65,	66,	67,	68,	69,	70,	71,
	72,	73,	74,	75,	76,	77,	78,	79,
{ corrected:
	81,	82,	83,	84,	85,	86,	87,	88,
   should be: }
	80,	82,	83,	84,	85,	86,	87,	88,
	89,	90,	91,	92,	93,	94,	95,	96,
	97,	98,	99,	100,	101,	102,	103,	104,
	105,	106,	107,	108,	109,	110,	111,	112,
	113,	114,	115,	116,	117,	118,	119,	120,
	121,	122,	123,	124,	125,	126,	127,	128);

  _a2u : array[0..127] of byte = (//			/* A- to u-law conversions */
	1,	3,	5,	7,	9,	11,	13,	15,
	16,	17,	18,	19,	20,	21,	22,	23,
	24,	25,	26,	27,	28,	29,	30,	31,
	32,	32,	33,	33,	34,	34,	35,	35,
	36,	37,	38,	39,	40,	41,	42,	43,
	44,	45,	46,	47,	48,	48,	49,	49,
	50,	51,	52,	53,	54,	55,	56,	57,
	58,	59,	60,	61,	62,	63,	64,	64,
	65,	66,	67,	68,	69,	70,	71,	72,
{ corrected:
	73,	74,	75,	76,	77,	78,	79,	79,
   should be: }
	73,	74,	75,	76,	77,	78,	79,	80,

	80,	81,	82,	83,	84,	85,	86,	87,
	88,	89,	90,	91,	92,	93,	94,	95,
	96,	97,	98,	99,	100,	101,	102,	103,
	104,	105,	106,	107,	108,	109,	110,	111,
	112,	113,	114,	115,	116,	117,	118,	119,
	120,	121,	122,	123,	124,	125,	126,	127);

function search(val: smallint;table: PByte;size: smallint):smallint;
var
  i: integer;
begin
	result:=size;
	for i := 0 to size-1 do begin
		if (val <= table[i])then begin
		  result:=i;
      break;
    end;
    inc(table);
	end;
end;

function KRPCM2ALaw(pcm_val: smallint):byte;//	/* 2's complement (16-bit range) */
var
  mask, seg: smallint;
  aval: byte;
begin
	pcm_val := mshr(pcm_val, 3);

	if (pcm_val >= 0) then begin
		mask := $D5;//		/* sign (7th) bit = 1 */
	end else begin
		mask := $55;//		/* sign bit = 0 */
		pcm_val := -pcm_val - 1;
	end;

	//* Convert the scaled magnitude to segment number. */
	seg := search(pcm_val, @seg_aend, 8);

	//* Combine the sign, segment, and quantization bits. */

	if (seg >= 8)then	begin	//* out of range, return maximum value. */
		result := ($7F xor mask);
  end else begin
		aval := seg shl SEG_SHIFT;
		if (seg < 2)then
			aval := aval or ( mshr(pcm_val, 1) and QUANT_MASK)
		else
			aval := aval or (mshr(pcm_val, seg) and QUANT_MASK);
		result:= (aval xor mask);
	end;
end;

function KRALaw2PCM(a_val: byte): smallint;
var
  t, seg: smallint;
begin
	a_val := a_val xor $55;
	t := (a_val and QUANT_MASK) shl 4;
	seg := mshr((word(a_val) and SEG_MASK), SEG_SHIFT);
	case (seg) of
  	0: t := t+8;
	  1: t := t+$108
  	else begin
	  	t := t+$108;
		  t := t shl (seg - 1);
    end;
  end;
	if (a_val and SIGN_BIT) <> 0 then result:=t else result:=-t;
end;

function KRPCM2ULaw(pcm_val: smallint):byte;//	/* 2's complement (16-bit range) */
var
  mask, seg: smallint;
  uval: byte;
begin
	//* Get the sign and the magnitude of the value. */
	pcm_val := mshr(pcm_val, 2);
	if (pcm_val < 0)then begin
		pcm_val := -pcm_val;
		mask := $7F;
	end else
		mask := $FF;

  if ( pcm_val > CLIP )then pcm_val := CLIP;		//* clip the magnitude */
	pcm_val := pcm_val + mshr(BIAS, 2);

	//* Convert the scaled magnitude to segment number. */
	seg := search(pcm_val, @seg_uend, 8);

	{
	 * Combine the sign, segment, quantization bits;
	 * and complement the code word.
	 *}
	if (seg >= 8)then		///* out of range, return maximum value. */
		result:= ($7F xor mask)
	else begin
		uval := (seg shl 4) or (mshr(pcm_val, (seg + 1)) and $F);
		result:= (uval xor mask);
	end;

end;

function KRULaw2PCM(u_val: byte): smallint;
var
	t: smallint;
begin
	//* Complement to obtain normal u-law value. */
	u_val := not u_val;

	{*
	 * Extract and bias the quantization bits. Then
	 * shift up by the segment number and subtract out the bias.
	 *}
	t := mshr((u_val and QUANT_MASK), 3) + BIAS;
	t := t shl mshr((u_val and SEG_MASK), SEG_SHIFT);

  if(u_val and SIGN_BIT)<>0 then result:=(BIAS - t) else result:=(t - BIAS);
end;

//* A-law to u-law conversion */
function KRALaw2ULaw(aval:byte):byte;
begin
	aval := aval and $ff;
	if(aval and $80)<>0 then result:=($FF xor _a2u[aval xor $D5])
	else result:= ($7F xor _a2u[aval xor $55]);
end;

//* u-law to A-law conversion */
function KRULaw2ALaw(uval:byte):byte;
begin
	uval := uval and $ff;
	if(uval and $80)<>0 then result:= ($D5 xor (_u2a[$FF xor uval] - 1))
  else result:=$55 xor (_u2a[$7F xor uval] - 1);
end;

end.
