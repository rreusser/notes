

// TypeScript declarations for @stdlib/lapack/base/zlansf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a complex symmetric matrix in RFP format.
	*/
	(
		norm: string,
		transr: string,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA: number,
		offsetA: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): number;
}

/**
* Returns the norm of a complex symmetric matrix in RFP format.
*/
declare var zlansf: Routine;

export = zlansf;
