

// TypeScript declarations for @stdlib/lapack/base/zhgeqz

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Implement the single-shift QZ method for computing generalized eigenvalues of a complex matrix pair in Hessenberg-triangular form.
	*/
	(
		job: string,
		compq: string,
		compz: string,
		N: number,
		ilo: number,
		ihi: number,
		H: Float64Array,
		strideH1: number,
		strideH2: number,
		offsetH: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		ALPHA: Float64Array,
		strideALPHA: number,
		offsetALPHA: number,
		BETA: Float64Array,
		strideBETA: number,
		offsetBETA: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Implement the single-shift QZ method for computing generalized eigenvalues of a complex matrix pair in Hessenberg-triangular form.
*/
declare var zhgeqz: Routine;

export = zhgeqz;
