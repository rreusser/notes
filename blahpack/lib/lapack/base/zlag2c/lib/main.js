
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlag2c = require( './zlag2c.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlag2c, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlag2c;
