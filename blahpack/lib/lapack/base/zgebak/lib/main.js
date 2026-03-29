'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgebak = require( './zgebak.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgebak, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgebak;
