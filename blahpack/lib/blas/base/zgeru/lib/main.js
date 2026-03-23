

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgeru = require( './zgeru.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeru, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeru;
