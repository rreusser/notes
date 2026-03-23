

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgelq2 = require( './dgelq2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgelq2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgelq2;
