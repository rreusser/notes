

// TypeScript declarations for @stdlib/lapack/base/dtrexc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reorders the real Schur factorization by an orthogonal similarity transformation
	*/
	(
		compq: string,
		N: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		ifst: number,
		ilst: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Reorders the real Schur factorization by an orthogonal similarity transformation
*/
declare var dtrexc: Routine;

export = dtrexc;
