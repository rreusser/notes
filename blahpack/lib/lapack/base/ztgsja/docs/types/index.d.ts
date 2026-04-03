

// TypeScript declarations for @stdlib/lapack/base/ztgsja

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the generalized singular value decomposition of two complex upper triangular matrices.
	*/
	(
		jobu: string,
		jobv: string,
		jobq: string,
		M: number,
		p: number,
		N: number,
		K: number,
		l: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		tola: number,
		tolb: number,
		ALPHA: Float64Array,
		strideALPHA: number,
		offsetALPHA: number,
		BETA: Float64Array,
		strideBETA: number,
		offsetBETA: number,
		U: Float64Array,
		strideU1: number,
		strideU2: number,
		offsetU: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		ncycle: number
	): Float64Array;
}

/**
* Computes the generalized singular value decomposition of two complex upper triangular matrices.
*/
declare var ztgsja: Routine;

export = ztgsja;
