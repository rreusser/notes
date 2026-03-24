

// TypeScript declarations for @stdlib/lapack/base/ztrsen

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reorder Schur factorization and compute condition numbers
	*/
	(
		job: string,
		compq: string,
		SELECT: Float64Array,
		strideSELECT: number,
		offsetSELECT: number,
		N: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		M: number,
		s: number,
		sep: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Reorder Schur factorization and compute condition numbers
*/
declare var ztrsen: Routine;

export = ztrsen;
