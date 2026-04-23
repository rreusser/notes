'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgebak = require( './dgebak.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgebak, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgebak;
