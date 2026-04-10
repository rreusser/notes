

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarfgp = require( './zlarfgp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarfgp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarfgp;
