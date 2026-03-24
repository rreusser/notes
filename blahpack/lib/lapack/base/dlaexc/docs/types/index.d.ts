

// TypeScript declarations for @stdlib/lapack/base/dlaexc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Swaps adjacent diagonal blocks of a real upper quasi-triangular matrix
	*/
	(
		wantq: boolean,
		N: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		j1: number,
		n1: number,
		n2: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Swaps adjacent diagonal blocks of a real upper quasi-triangular matrix
*/
declare var dlaexc: Routine;

export = dlaexc;
