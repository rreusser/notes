

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zpbtf2 = require( './zpbtf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpbtf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpbtf2;
