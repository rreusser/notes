

// TypeScript declarations for @stdlib/lapack/base/dtrsen

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reorders the Schur factorization and computes condition numbers
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
		WR: Float64Array,
		strideWR: number,
		offsetWR: number,
		WI: Float64Array,
		strideWI: number,
		offsetWI: number,
		M: number,
		s: number,
		sep: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		liwork: number
	): Float64Array;
}

/**
* Reorders the Schur factorization and computes condition numbers
*/
declare var dtrsen: Routine;

export = dtrsen;
