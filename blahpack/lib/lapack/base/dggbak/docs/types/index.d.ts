

// TypeScript declarations for @stdlib/lapack/base/dggbak

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Forms the right or left eigenvectors of a real generalized eigenvalue problem by backward transformation on the computed eigenvectors of the balanced matrix.
	*/
	(
		job: string,
		side: string,
		N: number,
		ilo: number,
		ihi: number,
		LSCALE: Float64Array,
		strideLSCALE: number,
		offsetLSCALE: number,
		RSCALE: Float64Array,
		strideRSCALE: number,
		offsetRSCALE: number,
		M: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number
	): Float64Array;
}

/**
* Forms the right or left eigenvectors of a real generalized eigenvalue problem by backward transformation on the computed eigenvectors of the balanced matrix.
*/
declare var dggbak: Routine;

export = dggbak;
