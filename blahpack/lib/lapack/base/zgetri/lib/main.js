

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgetri = require( './zgetri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetri;
