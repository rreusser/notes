

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zsytf2 = require( './zsytf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytf2;
