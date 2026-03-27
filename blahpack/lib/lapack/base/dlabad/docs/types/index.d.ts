

// TypeScript declarations for @stdlib/lapack/base/dlabad

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Take the square root of the overflow and underflow thresholds if the exponent range is very large.
	*/
	(
		small: number,
		large: number
	): void;
}

/**
* Take the square root of the overflow and underflow thresholds if the exponent range is very large.
*/
declare var dlabad: Routine;

export = dlabad;
