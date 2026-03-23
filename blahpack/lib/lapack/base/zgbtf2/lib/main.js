

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgbtf2 = require( './zgbtf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbtf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbtf2;
