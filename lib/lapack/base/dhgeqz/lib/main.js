
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dhgeqz = require( './dhgeqz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dhgeqz, 'ndarray', ndarray );


// EXPORTS //

module.exports = dhgeqz;
