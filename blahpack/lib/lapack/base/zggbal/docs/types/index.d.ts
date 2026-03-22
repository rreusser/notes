

// TypeScript declarations for @stdlib/lapack/base/zggbal

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Balance a pair of complex general matrices for the generalized eigenvalue problem.
	*/
	(
		job: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		ilo: number,
		ihi: number,
		LSCALE: Float64Array,
		strideLSCALE: number,
		offsetLSCALE: number,
		RSCALE: Float64Array,
		strideRSCALE: number,
		offsetRSCALE: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Balance a pair of complex general matrices for the generalized eigenvalue problem.
*/
declare var zggbal: Routine;

export = zggbal;
