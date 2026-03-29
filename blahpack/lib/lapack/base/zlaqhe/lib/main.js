'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaqhe = require( './zlaqhe.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqhe, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqhe;
