

// TypeScript declarations for @stdlib/lapack/base/ztrexc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reorder Schur factorization of a complex matrix
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
		ilst: number
	): Float64Array;
}

/**
* Reorder Schur factorization of a complex matrix
*/
declare var ztrexc: Routine;

export = ztrexc;
