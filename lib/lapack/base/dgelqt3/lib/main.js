
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgelqt3 = require( './dgelqt3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgelqt3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgelqt3;
